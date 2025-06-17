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

object HyperTypeChecker {

  val declaredVariables: Map[String, HyperType] = Map()

  var program : HHLProgram = HHLProgram(Seq.empty)


  def typeCheckProg(p: HHLProgram): Unit = {
      program = p
      program.content.foreach(m => typeCheckMethod(m))
  }

  def typeCheckMethod(m: Method): Unit = {
    // Type check the method body
    val pc = new HyperTypeCollection(Some(Low()))

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
      if (!declaredRetType.is_empty()) {
        
      val retType = finalMapping.getUnsafe(r.name)
      if (!retType.isSubTypeOf(declaredRetType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
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
          value = rightHyperType.value))

        val newCollection = delta.collection + (left.name -> deltaType)
        

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
            value = ty.value))
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
            val newMapping = mappingIf.combine(mappingElse)
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
        val (valueConditionType, deltaSub) = typeCheckExpression(mapping, delta, cond)
        if (valueConditionType.value == Some(False())) {
          // If the condition is false, we can skip the loop
          return (mapping, delta)
        }
        val initial_mapping = mapping
        var previous_mapping = mapping
        var newMapping = mapping

        var deltaMapping : Map[String,DeltaMapping] = Map();
        do {
          previous_mapping = newMapping
          val (condType, deltaCond) = typeCheckExpression(newMapping, delta, cond)
          val new_pc_inf = condType.joinInfFlow(pc);
          val new_pc = HyperTypeCollection(informationFlow=new_pc_inf)
          val res  = typeCheckStmt(newMapping, delta, body, new_pc)
          newMapping = res._1
          // deltaMapping = res._2
          newMapping = newMapping.combine(previous_mapping)
        } while (newMapping != previous_mapping)
        newMapping = previous_mapping.combine(initial_mapping)
        throw new Exception("While loop delta not implemented yet")
        return (newMapping, delta)
        }
      case HavocStmt(Id(name), _) => {
        val newMapping = mapping.set(name, HyperTypeCollection.fromSeq(Seq.empty))
        val newCollection = delta.collection + (name -> new DeltaMapping(Map(name -> HyperTypeCollection(informationFlow = Some(High()), value = Some(Zero())))))

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
        (new HyperTypeCollection(Some(Low()), value=value), DeltaMapping(Map()))
      }
      case Num(n) => {
        val value = if (n > 0) Some(Pos()) else if (n < 0) Some(Neg()) else Some(Zero())
        (new HyperTypeCollection(informationFlow=Some(Low()), value=value), DeltaMapping(Map()))
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
          val valueType = type1.joinValue(type2, op)
          valueType match {
            case Some(True()) | Some(False()) | Some(Zero()) => {
              infFlowType = Some(Low())
            }
            case _ => {}
          }
          val deltaMapping = DeltaMapping.combineOp(type1, delta1, type2, delta2, op)
          (new HyperTypeCollection(informationFlow = infFlowType, value = valueType), deltaMapping)
        }

      case LengthExpr(id) => {
        typeCheckExpression(mapping, delta, id)
      }
      case UnaryExpr(op, e) => {
        val (type1, deltaSub) = typeCheckExpression(mapping, delta, e)
        op match {
          case "-" => {
            // Negation
            val valueType = type1.value match {
              case Some(Pos()) => Some(Neg())
              case Some(Neg()) => Some(Pos())
              case Some(Zero()) => Some(Zero())
              case _ => None
            }
            return (new HyperTypeCollection(informationFlow = type1.informationFlow, value = valueType), deltaSub.flipSign())
          }
          case "!" => {
            // Negation of boolean
            val valueType = type1.value match {
              case Some(True()) => Some(False())
              case Some(False()) => Some(True())
              case _ => None
            }
            return (new HyperTypeCollection(informationFlow = type1.informationFlow, value = valueType), new DeltaMapping(Map()))
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
}