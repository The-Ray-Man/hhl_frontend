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

    val mapping = m.params.map(p => (p.name -> HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq())))).toMap
    val hyperMapping = new HyperMapping(mapping)

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
        val newMapping = mapping.set(left.name, HyperTypeCollection(
          informationFlow = infFlow,
          value = rightHyperType.value,
          mono = rightHyperType.mono))

        var newCollection = delta.collection 
        if (!deltaType.isEmpty()) {
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
            mono = ty.mono))
          newCollection = newCollection + (name.name -> new DeltaMapping(Map(name.name -> HyperTypeCollection(informationFlow = infFlow, value = None))))
        }
        return (newMapping, DeltaCollection(newCollection))
      }
      case CompositeStmt(stmts) => {
        val res =  stmts.foldLeft((mapping, delta))((acc, stmt) => {
          val (currentMapping, delta) = acc
          val (newMapping, newDelta) = typeCheckStmt(currentMapping, delta, stmt, pc)
          (newMapping, newDelta)
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
            val (mappingIf, deltaIf) = typeCheckStmt(mapping,delta, ifStmt, new_pc)
            val (mappingElse, deltaElse) = typeCheckStmt(mapping,delta, elseStmt, new_pc)
            println("mappingIf", mappingIf)
            val newMapping = mappingIf.combine(mappingElse, path_condition)
            val deltaNew = deltaIf.combine(deltaElse)

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
        // TODO: Very likely unsound!
        val (valueConditionType, deltaSub) = typeCheckExpression(mapping, delta, cond)
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
          val (condType, deltaCondition) = typeCheckExpression(newMapping, DeltaCollection(Map()), cond)
          println("delta type of condition", deltaCondition)
          val new_pc_inf = condType.joinInfFlow(pc);
          val new_pc = HyperTypeCollection(informationFlow=new_pc_inf)
          val res  = typeCheckStmt(newMapping, delta, body, new_pc)
          newMapping = res._1
          val resDelta = typeCheckStmt(mappingForDelta, delta, body, pc)
          mappingForDelta = resDelta._1
          deltas = deltas :+ resDelta._2
          newMapping = newMapping.combine(previous_mapping, new_pc_inf)
        } while (newMapping != previous_mapping)
        newMapping = previous_mapping.combine(initial_mapping, valueConditionType.informationFlow)

        // Some logic here for monotonicity stuff.
        bodyDelta = deltas.reduce((d1, d2) => d1.combine(d2))
       
        // Try to find monotonicity in number of while loop iterations.

        // Variables that are always increasing/decreasing in every loop iteration.
        val monotonValues = bodyDelta.collection.filter { case (name, value) =>
          value.mapping.get(name) match {
            case Some(collection) =>  collection.informationFlow == Low() && (collection.value == Some(Pos()) || collection.value == Some(Neg()))
            case None => false
          }
        }.map {case (name, value) => name }.toSet

        // Variables that are present in the condition of the while loop.
        val conditionVariables = getVariables(cond)

        val toConsider = monotonValues.intersect(conditionVariables)

        if (toConsider.isEmpty){
          return (newMapping, delta)
        }

        // Now we need to check if the number of loop iterations is monotonic to one of the variables in `toConsider`.
        // One side of the condition needs to be constant i.e. not changed in the loop body. The other side of the condition must be increasing/decreasing.

        val (lhs, op, rhs) = cond match {
          case BinaryExpr(e1, op, e2) => (e1, op, e2)
          case _ => return (newMapping, delta) // Not a binary expression, cannot check monotonicity
        }

        // The types in this mapping should be align with every loop iteration. If the type of a variable is low, it means that in every loop iteration every trace has the same value.
        val conditionMapping = initial_mapping.mapping.map( {case (name, collection) => {
          if (bodyDelta.collection.contains(name)) {
            val deltaCollection = bodyDelta.collection(name).mapping.get(name) match {
              case Some(value) => value
              case None => HyperTypeCollection(informationFlow = High(), value = None)
            }

            val infFlow = collection.joinInfFlow(deltaCollection)
            val value = (collection.value, deltaCollection.value) match {
              case (Some(Pos()), Some(Pos())) => Some(Pos())
              case (Some(Neg()), Some(Neg())) => Some(Neg())
              case (Some(Zero()), Some(Zero())) => Some(Zero())
              case _ => None
            }
            (name, HyperTypeCollection(informationFlow = infFlow, value = value))
          } else {
            (name, collection) // Is this wrong?
          }
        }}
        ).toMap

        val tmpMapping = new HyperMapping(conditionMapping)


        // Now we need to find the baseId (to which the monotonicity applies) and the id (which is increasing/decreasing).
        val lhsChange = changeOfExpression(lhs, tmpMapping, bodyDelta)
        val rhsChange = changeOfExpression(rhs, tmpMapping, bodyDelta)

        val parameter = this.method match {
          case Some(m) => m.params.map(_.name).toSet
          case None => Set.empty[String]
        }

        val lhsDpendentOnVars = getVariables(lhs).intersect(parameter)
        val rhsDpendentOnVars = getVariables(rhs).intersect(parameter)

        val lhsCandidateVariable = if  (lhsDpendentOnVars.size == 1) {
          Some(lhsDpendentOnVars.head)
        } else {
          None
        }

        val rhsCandidateVariable = if (rhsDpendentOnVars.size == 1) {
          Some(rhsDpendentOnVars.head)
        } else {
          None
        }

        // This means the number of loop iterations is monotonic.
        val monotonicity = (lhsChange, lhsCandidateVariable, rhsChange, rhsCandidateVariable, op) match {
          case ("constant", Some(lhsVar), "increasing", _, ">=") => {
            Some(MonoUp(Id(lhsVar)))
          }
          case ("constant", Some(lhsVar), "increasing", _, ">") => {
            Some(MonoUp(Id(lhsVar)))
          }
          case ("constant", Some(lhsVar), "decreasing", _, "<=") => {
            Some(MonoDown(Id(lhsVar)))
          }
          case ("constant", Some(lhsVar), "decreasing", _, "<") => {
            Some(MonoDown(Id(lhsVar)))
          }
          case ("increasing", _, "constant", Some(rhsVar), "<=") => {
            Some(MonoUp(Id(rhsVar)))
          }
          case ("increasing", _, "constant", Some(rhsVar), "<") => {
            Some(MonoUp(Id(rhsVar)))
          }
          case ("decreasing", _, "constant", Some(rhsVar), ">=") => {
            Some(MonoDown(Id(rhsVar)))
          }
          case ("decreasing", _, "constant", Some(rhsVar), ">") => {
            Some(MonoDown(Id(rhsVar)))
          }
          case _ => None
        }
        println("monotonicity", monotonicity)

        if (monotonicity.isDefined) {
          // After the loop, all variables which were before low, and every loop iteration has the same effect i.e. increasing/decresing with low, will become monotonic.
          val newMappingWithMonotonicity = initial_mapping.mapping.map { case (name, collection) =>
            if (collection.informationFlow == High()) {
              (name, newMapping.getUnsafe(name))
            } else {
              bodyDelta.collection.get(name) match {
                case Some(changeInLoop) => {
                  changeInLoop.mapping.get(name) match {
                    case Some(value) if value.informationFlow == Low() && (value.value == Some(Pos()) || value.value == Some(Neg())) => {
                      val baseHyperTypeCollection = newMapping.getUnsafe(name)
                      (monotonicity, value.value) match {
                        case (Some(MonoUp(baseId)), Some(Pos())) => {
                          // If the variable is monotonic and increasing, we set the mapping to the new value.
                          (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = Some(MonoUp(baseId))))
                        }
                        case (Some(MonoDown(baseId)), Some(Neg())) => {
                          // If the variable is monotonic and decreasing, we set the mapping to the new value.
                          (name, HyperTypeCollection(informationFlow = baseHyperTypeCollection.informationFlow, value = baseHyperTypeCollection.value, mono = Some(MonoDown(baseId))))
                        }
                        case _ => {
                          // If the variable is not monotonic, we keep the old mapping.
                          (name, newMapping.getUnsafe(name))
                      }
                      }
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
          return (HyperMapping(newMappingWithMonotonicity), bodyDelta.combine(delta))
        } else {
          // No monotonicity found, return the mapping and delta as is.
          return (newMapping, delta)
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

  def changeOfExpression(expr: Expr, mapping: HyperMapping, changeInLoopIteration: DeltaCollection): String  = {
    val dependent = typeCheckExpression(mapping, DeltaCollection(Map()), expr)._2

      val res = dependent.mapping.foldLeft("constant"){ case ( acc, (key, value)) =>
        if (changeInLoopIteration.collection.contains(key)) {
          val change = changeInLoopIteration.collection(key).mapping.get(key) match {
            case Some(v) => v
            case None => HyperTypeCollection(informationFlow = High(), value = None)
          }
          if (value.informationFlow == High() || change.informationFlow == High()) {
            "unknown"
          } else {
            val currentDirection = change.value match {
              case Some(Pos()) => "increasing"
              case Some(Neg()) => "decreasing"
              case Some(Zero()) => "constant"
              case None => "unknown"
            }
            val combined = (acc, currentDirection) match {
              case ("unknown", _) => "unknown"
              case (_, "unknown") => "unknown"
              case ("constant", "increasing") => "increasing"
              case ("constant", "decreasing") => "decreasing"
              case ("constant", "constant") => "constant"
              case ("increasing", "decreasing") => "unknown"
              case ("increasing", "increasing") => "increasing"
              case ("increasing", "constant") => "increasing"
              case ("decreasing", "decreasing") => "decreasing"
              case ("decreasing", "increasing") => "unknown"
              case ("decreasing", "constant") => "decreasing"
              case _ => "unknown"
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
        (new HyperTypeCollection(informationFlow=Low(), value=value), DeltaMapping(Map()))
      }
      case Id(name) => {
        val hyperType = mapping.getUnsafe(name)
        if (delta.collection.contains(name)) {
          return (hyperType, delta.collection(name))
        } else {
          return (hyperType, new DeltaMapping(Map(name -> new HyperTypeCollection(informationFlow = hyperType.informationFlow, value = Some(Zero())))))
        }
        (hyperType, new DeltaMapping(Map(name -> new HyperTypeCollection(informationFlow = hyperType.informationFlow, value = Some(Zero())))))
      }
      case BinaryExpr(e1, op, e2) => 
        {
          val (type1, delta1) = typeCheckExpression(mapping,delta, e1)
          val (type2, delta2) = typeCheckExpression(mapping,delta, e2)
          var infFlowType = type1.joinInfFlow(type2)
          val valueType = type1.combineValueOp(type2, op)
          val monoType = type1.combineMonoOp(type2, op)
          valueType match {
            case Some(True()) | Some(False()) | Some(Zero()) => {
              infFlowType = Low()
            }
            case _ => {}
          }
          val deltaMapping = DeltaMapping.combineOp(type1, delta1, type2, delta2, op)
          (new HyperTypeCollection(informationFlow = infFlowType, value = valueType, mono = monoType), deltaMapping)
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