package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.ast.IfElseStmt
import viper.HHLVerifier.ast.UnfoldStmt
import viper.HHLVerifier.ast.FoldStmt
import viper.HHLVerifier.ast.HavocStmt
import viper.HHLVerifier.ast.WhileLoopStmt
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.PVarDecl
import viper.HHLVerifier.ast.MultiAssignStmt
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast.HyperAssumeStmt
import viper.HHLVerifier.typing.HyperType
import viper.HHLVerifier.typing.MonoTypeCollection

object HyperTypeChecker {

  val declaredVariables: Map[String, HyperType] = Map()

  var program : HHLProgram = HHLProgram(Seq.empty)

  var method : Option[Method] = None 


  def typeCheckProg(p: HHLProgram): Unit = {
      program = p
      program.content.foreach(m => {this.method = Some(m); typeCheckMethod(m)})
  }

  def typeCheckMethod(m: Method): Unit = {
    // Type check the method body
    val pc = new HyperTypeCollection(informationFlow = Low())

    val mapping = m.params.map(p => (p.name -> {
      val baseCollection = HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq()))
      if (baseCollection.mono.isEmpty && baseCollection.informationFlow== High()) {
        HyperTypeCollection(informationFlow = High(), value = baseCollection.value, mono = Some(MonoTypeCollection(MonoUp(Set(Id(p.name))))), absValue = baseCollection.absValue)
      } else {
        baseCollection
      }
    })).toMap
    val hyperMapping = new HyperMapping(mapping)

    println("mapping at method start\n", hyperMapping)

    val (finalMapping, deltaMapping) = typeCheckStmt(hyperMapping, DeltaCollection(Map()), m.body, pc)
    println("final mapping", finalMapping.mapping)
    println("delta mapping")
    deltaMapping.collection.foreach { case (key, value) =>
      println(s"$key: ${value.mapping}")
    }
    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      val retType = finalMapping.getUnsafe(r.name)
      if (!retType.isSubTypeOf(declaredRetType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
    })
  }

  def typeCheckStmt(mapping:  HyperMapping, delta: DeltaCollection, s : Stmt, pc: HyperTypeCollection) : (HyperMapping, DeltaCollection) = {
    s match {
      case AssignStmt(left, right) => {
        val (rightHyperType, deltaType) = typeCheckExpression(mapping, delta ,right)
        val infFlow = rightHyperType.joinInfFlow(pc)
        val value = rightHyperType.value
        val mono = rightHyperType.mono
        val absValue = rightHyperType.absValue

        val newMapping = mapping.set(left.name, HyperTypeCollection(
          informationFlow = infFlow,
          value = value,
          mono = mono, 
          absValue = absValue)
        )

        var newCollection = delta.collection 
        if (!deltaType.isEmpty) {
          newCollection = newCollection + (left.name -> deltaType)
        } 
        
        return (newMapping, DeltaCollection(newCollection))
      }
      case MultiAssignStmt(left, right) => {
        val rightHyperType = typeCheckMethodExpr(mapping, delta, right)
        var newMapping = mapping;
        var newCollection = delta.collection;
        for ((name, ty) <- left.zip(rightHyperType)) {
          val infFlow = ty.joinInfFlow(pc)
          newMapping = newMapping.set(name.name, HyperTypeCollection(
            informationFlow = infFlow,
            value = ty.value,
            mono = ty.mono,
            absValue = ty.absValue))
          newCollection = newCollection + (name.name -> new DeltaMapping(Map(name.name -> HyperTypeCollection(informationFlow = infFlow, value = None))))
        }
        return (newMapping, DeltaCollection(newCollection))
      }
      case CompositeStmt(stmts) => {
        val res =  stmts.foldLeft((mapping, delta))((acc, stmt) => {
          val (currentMapping, delta) = acc
          val (newMapping, newDelta) = typeCheckStmt(currentMapping, DeltaCollection(Map.empty), stmt, pc)
          val combinedDelta = delta.concat(newDelta)
          (newMapping, combinedDelta)
          })
        return res
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val (condType, _) = typeCheckExpression(mapping, delta, cond)
        condType.value match {
          case Some(True()) => {
            // If the condition is true, we only need to consider the if branch
            typeCheckStmt(mapping, delta, ifStmt, HyperTypeCollection(informationFlow=pc.informationFlow))
          }
          case Some(False()) => {
            // If the condition is false, we only need to consider the else branch
            return typeCheckStmt(mapping, delta, elseStmt, HyperTypeCollection(informationFlow=pc.informationFlow))
          }
          case None => {
            // If the condition is unknown, we need to consider both branches
            // We will join the information flow of both branches
            val path_condition = condType.joinInfFlow(pc);
            val new_pc = HyperTypeCollection(informationFlow=path_condition)
            val (mappingIf, deltaIf) = typeCheckStmt(mapping, DeltaCollection(Map.empty), ifStmt, new_pc)
            val (mappingElse, deltaElse) = typeCheckStmt(mapping, DeltaCollection(Map.empty), elseStmt, new_pc)
            val newMapping = mappingIf.combine(mappingElse, path_condition)
            val deltaBranch = deltaIf.combine(deltaElse, condType)

            val deltaNew = delta.concat(deltaBranch)

            return (newMapping, deltaNew) // TODO
          }
        }


      }
      case UnfoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val var_type = mapping.getUnsafe(id.name)
        if (var_type.isSubTypeOf(ty)) {
          return (mapping, delta)
        } else {
          throw new Exception("Type error: cannot unfold " + id.name + " of type " + var_type + " to type " + t)
        }
      }
      case FoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val newMapping = mapping.set(id.name, ty)
        (newMapping, delta)
      }
      case WhileLoopStmt(cond, body, _, _, _) => {
        var (valueConditionType, deltaSub) = typeCheckExpression(mapping, delta, cond)
        if (valueConditionType.value == Some(False())) {
          // If the condition is false, we can skip the loop
          return (mapping, delta)
        }


        val initial_mapping = mapping
        var previous_mapping = mapping
        var newMapping = mapping

        var mappingForDelta = mapping
        var deltas = Seq.empty[DeltaCollection]
        var bodyDelta = DeltaCollection(Map())
        do {
          previous_mapping = newMapping
          val (condType, _) = typeCheckExpression(newMapping, DeltaCollection(Map()), cond)
          val new_pc_inf = condType.joinInfFlow(pc);
          valueConditionType = HyperTypeCollection(informationFlow=new_pc_inf)
          val res  = typeCheckStmt(newMapping, delta, body, valueConditionType)
          newMapping = res._1
          val resDelta = typeCheckStmt(mappingForDelta, DeltaCollection(Map()), body, pc)
          mappingForDelta = resDelta._1
          deltas = deltas :+ resDelta._2
          newMapping = newMapping.combine(previous_mapping, valueConditionType.informationFlow)
        } while (newMapping != previous_mapping)
        newMapping = previous_mapping.combine(initial_mapping, valueConditionType.informationFlow)

        // We need to compute how the delta changes in the body of the loop.
        bodyDelta = deltas.reduce((d1, d2) => d1.concat(d2))
       

        val loopMono = HyperTypeChecker.findMonotonicityOfLoop(cond, initial_mapping, bodyDelta)
        println("Monotonicity of loop condition", loopMono)
        // // Variables that are present in the condition of the while loop.
        
        if (loopMono.nonEmpty) {
          // After the loop, all variables which were before low, and every loop iteration has the same effect i.e. increasing/decresing with low, will become monotonic.
          val newMappingWithMonotonicity = initial_mapping.mapping.map { case (name, collection) =>
            if (collection.informationFlow == High()) {
              // If the value was monotonic before, and the loop condition is monotonic as well, we may be able to keep the monotonicity.

              if (collection.mono.nonEmpty) {
                bodyDelta.collection.get(name) match {
                  case Some(changeInLoopMapping) => {
                    val changeInLoop = changeInLoopMapping.mapping.get(name).get
                    val baseHyperTypeCollection = newMapping.getUnsafe(name)
                    (collection.mono.get.mono, loopMono.get.mono, changeInLoop.informationFlow, changeInLoop.value) match {
                      case (MonoUp(beforeLoop), MonoUp(loopMonoVars), Low(), Some(Pos())) if beforeLoop == loopMonoVars => {
                        (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = loopMono))
                      }
                      case (MonoDown(beforeLoop), MonoDown(loopMonoVars), Low(), Some(Pos())) if beforeLoop == loopMonoVars => {
                        (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = loopMono))
                      }
                      case (MonoUp(beforeLoop), MonoDown(loopMonoVars), Low(), Some(Neg())) if beforeLoop == loopMonoVars => {
                        (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = Some(MonoTypeCollection(MonoUp(beforeLoop)))))
                      }
                      case (MonoDown(beforeLoop), MonoUp(loopMonoVars), Low(), Some(Neg())) if beforeLoop == loopMonoVars => {
                        (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = Some(MonoTypeCollection(MonoDown(beforeLoop)))))
                      }
                      case _ => {
                        // Here we could add even more cases
                        (name, newMapping.getUnsafe(name))
                      }

                    }
                  }
                  case None => {
                    (name, newMapping.getUnsafe(name))
                  }
                }
              } else {
              (name, newMapping.getUnsafe(name))
              }
            } else {
              bodyDelta.collection.get(name) match {
                case Some(changeInLoop) => {
                  changeInLoop.mapping.get(name) match {
                    case Some(value) => {
                      if (value.informationFlow == Low() && (value.value == Some(Pos()) || value.value == Some(Neg()))) {
                        val baseHyperTypeCollection = newMapping.getUnsafe(name)
                        value.value match {
                          case (Some(Pos())) => {
                            (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = loopMono))
                          }
                          case (Some(Neg())) => {
                            (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = Some(loopMono.get.flip)))
                          }
                          case _ => {
                            // If the variable is not monotonic, we keep the old mapping.
                            (name, newMapping.getUnsafe(name))
                          }
                        }
                      } else if (value.mono.nonEmpty) {
                        val baseHyperTypeCollection = newMapping.getUnsafe(name)
                        (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = loopMono.get.union(value.mono.get)))
                      }
                      else (
                        (name, newMapping.getUnsafe(name))
                      )
                    }
                    case _ => {
                      // If the variable is not monotonic, we keep the old mapping.
                      (name, newMapping.getUnsafe(name))
                    }
                  }
                }
                case None => {
                  (name, newMapping.getUnsafe(name))
                }
              }
            }
          }
          return (HyperMapping(newMappingWithMonotonicity), bodyDelta.combine(delta, valueConditionType))
        } else {
          // No monotonicity found, return the mapping and delta as is.
          return (newMapping, bodyDelta.combine(delta, valueConditionType))
        }
      }

      case HavocStmt(Id(name), _) => {
        val newMapping = mapping.set(name, HyperTypeCollection.fromSeq(Seq.empty))
        val newCollection = delta.collection + (name -> new DeltaMapping(Map(name -> HyperTypeCollection(informationFlow = High(), value = Some(Zero())))))

        return (newMapping, DeltaCollection(newCollection))
      }
      
      case PVarDecl(_, _) => {
        return (mapping, delta)
      }

      case HyperAssertStmt(e) => {
        return (mapping, delta)
      } 
      case HyperAssumeStmt(e) => {
        return (mapping,delta)
      }

      case _ => {
        throw new Exception("Type error: cannot yet type check statement " + s)
      }
    }
  }

  def findMonotonicityOfLoop(loopGuard : Expr, beforeLoopMapping : HyperMapping, bodyChange : DeltaCollection) : Option[MonoTypeCollection] = {
    println("beforeLoopMapping", beforeLoopMapping)
    println("bodyChange", bodyChange)
    val (lhs, op, rhs) = loopGuard match {
      case BinaryExpr(e1, op, e2) => (e1, op, e2)
      case _ => throw new Exception("(This should never happen) Loop guard is not a binary expression: " + loopGuard)
    }

    val lhsChange = changeOfExpression(lhs, beforeLoopMapping, bodyChange)
    val rhsChange = changeOfExpression(rhs, beforeLoopMapping, bodyChange)

    val lhsDelta = typeCheckExpression(beforeLoopMapping, DeltaCollection(Map()), lhs)
    val rhsDelta = typeCheckExpression(beforeLoopMapping, DeltaCollection(Map()), rhs)

    (lhsChange, rhsChange) match {
      case (Some(Pos()), Some(Zero())) => {
        println("lhs is increasing, rhs is constant")
        if (op == "<" || op == "<=") {
          return Some(MonoTypeCollection(MonoUp(rhsDelta._2.mapping.keySet.map(key => Id(key)))))
        }
      }
      case (Some(Neg()), Some(Zero())) => {
        println("lhs is decreasing, rhs is constant")
        if (op == ">" || op == ">=") {
          return Some(MonoTypeCollection(MonoDown(rhsDelta._2.mapping.keySet.map(key => Id(key)))))
        }
      }
      case (Some(Zero()), Some(Pos())) => {
        println("lhs is constant, rhs is increasing")
        if (op == ">" || op == ">=") {
          return Some(MonoTypeCollection(MonoUp(lhsDelta._2.mapping.keySet.map(key => Id(key)))))
        }
      }
      case (Some(Zero()), Some(Neg())) => {
        println("lhs is constant, rhs is decreasing")
        if (op == "<" || op == "<=") {
          return Some(MonoTypeCollection(MonoDown(lhsDelta._2.mapping.keySet.map(key => Id(key)))))
        }
      }
      case (_,_) => {
        return None
      }
    }
    return None
  }

  def changeOfExpression(expr: Expr, mapping: HyperMapping, changeInLoopIteration: DeltaCollection) : Option[HyperType]  = {
    val dependent = typeCheckExpression(mapping, DeltaCollection(Map()), expr)._2

      val res = dependent.mapping.foldLeft(Some(Zero()) : Option[HyperType]){ case ( acc, (key, value)) =>
        if (changeInLoopIteration.collection.contains(key)) {
          val change = changeInLoopIteration.collection(key).mapping.get(key) match {
            case Some(v) => v
            case None => HyperTypeCollection(informationFlow = High(), value = None)
          }
          if (value.informationFlow == High() || change.informationFlow == High()) {
            None
          } else {
            val currentDirection = change.value 
            val combined : Option[HyperType] = (acc, currentDirection) match {
              case (None, _) => None
              case (_, None) => None
              case (Some(Zero()), Some(Pos())) => Some(Pos())
              case (Some(Zero()), Some(Neg())) => Some(Neg())
              case (Some(Zero()), Some(Zero())) => Some(Zero())
              case (Some(Pos()), Some(Neg())) => None
              case (Some(Pos()), Some(Pos())) => Some(Pos())
              case (Some(Pos()), Some(Zero())) => Some(Pos())
              case (Some(Neg()), Some(Neg())) =>  Some(Neg())
              case (Some(Neg()), Some(Pos())) => None
              case (Some(Neg()), Some(Zero())) => Some(Neg())
              case _ => None
            }
            combined
          }
        } else {
          acc
        }
      }
      res
  }
  

  def typeCheckMethodExpr(mapping: HyperMapping,delta : DeltaCollection, e: MethodCallExpr) : Seq[HyperTypeCollection] = {
    val method = program.methods.find(_.mName == e.methodName) match {
      case None => throw new Exception("Method not found: " + e.methodName + " in " + program.methods.map(_.mName).mkString(", "))
      case Some(value) => value
    }
    val methodArgTypes = method.params.map(p => HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq())))
   
    for ((name, ty) <- e.args.zip(methodArgTypes)) {
      val (actual_type, _) = typeCheckExpression(mapping,delta, name)
      if (!actual_type.isSubTypeOf(ty)) {
        throw new Exception("Type error: argument " + name + " of type " + actual_type + " does not match expected type " + ty)
      }
    }
    method.res.map(r => HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq())))
  }


  def typeCheckExpression(mapping: HyperMapping, delta : DeltaCollection, e: Expr) : (HyperTypeCollection, DeltaMapping) = {
    e match {
      case BoolLit(b) => {
        val value = if (b) Some(True()) else Some(False())
        (new HyperTypeCollection(informationFlow=Low(), value=value), DeltaMapping(Map()))
      }
      case Num(n) => {
        val value = if (n > 0) Some(Pos()) else if (n < 0) Some(Neg()) else Some(Zero())
        val absValue = if (n < 1 && n > -1) Some(LessOne()) else if (n==1 || n == -1) Some(One()) else Some(GreaterOne())
        (new HyperTypeCollection(informationFlow=Low(), value=value, absValue = absValue), DeltaMapping(Map()))
      }
      case Id(name) => {
        val hyperType = mapping.getUnsafe(name)
        if (delta.collection.contains(name)) {
          return (hyperType, delta.collection(name))
        } else {
          return (hyperType, new DeltaMapping(Map(name -> new HyperTypeCollection(informationFlow = Low(), value = Some(Zero())))))
        }
      }
      case BinaryExpr(e1, op, e2) => 
        {
          val (type1, delta1) = typeCheckExpression(mapping,delta, e1)
          val (type2, delta2) = typeCheckExpression(mapping,delta, e2)
          var infFlowType = type1.joinInfFlow(type2)
          val valueType = type1.combineValueOp(type2, op)
          val monoType = type1.combineMonoOp(type2, op)
          val absType = type1.combineAbsValueOp(type2, op)
          valueType match {
            case Some(True()) | Some(False()) | Some(Zero()) => {
              infFlowType = Low()
            }
            case _ => {}
          }
          if (absType.isDefined && absType.get == One() && (valueType.isDefined && (valueType.get == Pos() || valueType.get == Neg()))) {
            // The value must be either 1 or -1, hence the information flow is low.
            infFlowType = Low()
          }
        
          val deltaMapping = DeltaMapping.combineOp(type1, delta1, type2, delta2, op)
          (new HyperTypeCollection(informationFlow = infFlowType, value = valueType, mono = monoType, absValue = absType), deltaMapping)
        }

      case LengthExpr(id) => {
        typeCheckExpression(mapping, delta, id)
      }
      case UnaryExpr(op, e) => {
        op match {
          case "-" => {
            return typeCheckExpression(mapping, delta, BinaryExpr(Num(0), "-", e))
          }
          case "!" => {
            // Negation of boolean
            return typeCheckExpression(mapping, delta, BinaryExpr(e, "==", BoolLit(false)))
          }
          case _ => throw new Exception("Unknown unary operator: " + op)
        }
      }
      case LookupExpr(id, index) => {
        val (idType, _) = typeCheckExpression(mapping, delta, id)
        val (indexType, _) = typeCheckExpression(mapping,delta, index)
        val infFlowType = idType.joinInfFlow(indexType)
        val valueType = idType.value
        (new HyperTypeCollection(informationFlow = infFlowType, value = valueType), new DeltaMapping(Map()))
      }
      case _ => {
        throw new Exception("Type error: cannot yet type check expression " + e)
      }
     }
  }

  def getVariables(expr: Expr) : Set[String] = {
    expr match {
      case Id(name) => Set(name)
      case BinaryExpr(e1, _, e2) => getVariables(e1) ++ getVariables(e2)
      case UnaryExpr(_, e) => getVariables(e)
      case LookupExpr(id, index) => getVariables(id) ++ getVariables(index)
      case LengthExpr(id) => getVariables(id)
      case _ => {Set.empty[String]}
    }
  }
}