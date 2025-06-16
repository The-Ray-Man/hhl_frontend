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

    val finalMapping = typeCheckStmt(hyperMapping, m.body, pc)
    println(finalMapping.mapping)
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

  def typeCheckStmt(mapping:  HyperMapping, s : Stmt, pc: HyperTypeCollection) : HyperMapping = {
    println("Type checking statement: " + s)
    s match {
      case AssignStmt(left, right) => {
         val rightHyperType = typeCheckExpression(mapping, right)
         val infFlow = rightHyperType.joinInfFlow(pc)
         val newMapping = mapping.set(left.name, HyperTypeCollection(
          informationFlow = infFlow,
          value = rightHyperType.value))
         return newMapping
      }
      case MultiAssignStmt(left, right) => {
        val rightHyperType = typeCheckMethodExpr(mapping, right)
        var newMapping = mapping;
        for ((name, ty) <- left.zip(rightHyperType)) {
          val infFlow = ty.joinInfFlow(pc)
          newMapping = newMapping.set(name.name, HyperTypeCollection(
            informationFlow = infFlow,
            value = ty.value))
        }
        return newMapping
      }
      case CompositeStmt(stmts) => {
        return stmts.foldLeft(mapping)((acc, stmt) => typeCheckStmt(acc, stmt, pc))
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val condType = typeCheckExpression(mapping, cond)
        println(s"Condition type: $condType")
        condType.value match {
          case Some(True()) => {
            // If the condition is true, we only need to consider the if branch
            return typeCheckStmt(mapping, ifStmt, HyperTypeCollection(informationFlow=pc.informationFlow))
          }
          case Some(False()) => {
            // If the condition is false, we only need to consider the else branch
            return typeCheckStmt(mapping, elseStmt, HyperTypeCollection(informationFlow=pc.informationFlow))
          }
          case None => {
            // If the condition is unknown, we need to consider both branches
            // We will join the information flow of both branches
            val path_condition = condType.joinInfFlow(pc);
            val new_pc = HyperTypeCollection(informationFlow=path_condition)
            val mappingIf = typeCheckStmt(mapping, ifStmt, new_pc)
            val mappingElse = typeCheckStmt(mapping, elseStmt, new_pc)
            val newMapping = mappingIf.combine(mappingElse)
            return newMapping
          }
        }


      }
      case UnfoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val var_type = mapping.getUnsafe(id.name)
        if (var_type.isSubTypeOf(ty)) {
          return mapping
        } else {
          throw new Exception("Type error: cannot unfold " + id.name + " of type " + var_type + " to type " + t)
        }
      }
      case FoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val newMapping = mapping.set(id.name, ty)
        newMapping
      }
      case WhileLoopStmt(cond, body, _, _, _) => {
        val valueConditionType = typeCheckExpression(mapping, cond)
        if (valueConditionType.value == Some(False())) {
          // If the condition is false, we can skip the loop
          return mapping
        }
        val initial_mapping = mapping
        var previous_mapping = mapping
        var newMapping = mapping
        do {
          previous_mapping = newMapping
          val condType = typeCheckExpression(newMapping, cond)
          val new_pc_inf = condType.joinInfFlow(pc);
          val new_pc = HyperTypeCollection(informationFlow=new_pc_inf)
          newMapping = typeCheckStmt(newMapping, body, new_pc)
          newMapping = newMapping.combine(previous_mapping)
        } while (newMapping != previous_mapping)

        newMapping = previous_mapping.combine(initial_mapping)
        return newMapping
        }
      case HavocStmt(id, _) => {
        val newMapping = mapping.set(id.name, HyperTypeCollection.fromSeq(Seq.empty))
        return newMapping
      }
      
      case PVarDecl(_, _) => {
        return mapping
      }

      case HyperAssertStmt(e) => {
        return mapping
      } 
      case HyperAssumeStmt(e) => {
        return mapping
      }

      case _ => {
        throw new Exception("Type error: cannot yet type check statement " + s)
      }
    }
  }

  def typeCheckMethodExpr(mapping: HyperMapping, e: MethodCallExpr) : Seq[HyperTypeCollection] = {
    val method = program.methods.find(_.mName == e.methodName) match {
      case None => throw new Exception("Method not found: " + e.methodName + " in " + program.methods.map(_.mName).mkString(", "))
      case Some(value) => value
    }
    val methodArgTypes = method.params.map(p => HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq())))
   
    for ((name, ty) <- e.args.zip(methodArgTypes)) {
      val actual_type = typeCheckExpression(mapping, name)
      if (!actual_type.isSubTypeOf(ty)) {
        throw new Exception("Type error: argument " + name + " of type " + actual_type + " does not match expected type " + ty)
      }
    }
    method.res.map(r => HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq())))
  }


  def typeCheckExpression(mapping: HyperMapping, e: Expr) : HyperTypeCollection = {
    println("Type checking expression: " + e)
    e match {
      case BoolLit(b) => {
        val value = if (b) Some(True()) else Some(False())
        new HyperTypeCollection(Some(Low()), value=value)
      }
      case Num(n) => {
        val value = if (n > 0) Some(Pos()) else if (n < 0) Some(Neg()) else Some(Zero())
        new HyperTypeCollection(informationFlow=Some(Low()), value=value)
      }
      case Id(name) => {
        mapping.getUnsafe(name)
      }
      case BinaryExpr(e1, op, e2) => 
        {
          val type1 = typeCheckExpression(mapping, e1)
          val type2 = typeCheckExpression(mapping, e2)
          var infFlowType = type1.joinInfFlow(type2)
          val valueType = type1.joinValue(type2, op)
          println("infFlowType: " + infFlowType + ", valueType: " + valueType)
          valueType match {
            case Some(True()) | Some(False()) | Some(Zero()) => {
              infFlowType = Some(Low())
            }
            case _ => {}
          }
          new HyperTypeCollection(informationFlow = infFlowType, value = valueType)
        }

      case LengthExpr(id) => {
        typeCheckExpression(mapping, id)
      }
      case UnaryExpr(op, e) => {
        val type1 = typeCheckExpression(mapping, e)
        op match {
          case "-" => {
            // Negation
            val valueType = type1.value match {
              case Some(Pos()) => Some(Neg())
              case Some(Neg()) => Some(Pos())
              case Some(Zero()) => Some(Zero())
              case _ => None
            }
            return new HyperTypeCollection(informationFlow = type1.informationFlow, value = valueType)
          }
          case "!" => {
            // Negation of boolean
            val valueType = type1.value match {
              case Some(True()) => Some(False())
              case Some(False()) => Some(True())
              case _ => None
            }
            return new HyperTypeCollection(informationFlow = type1.informationFlow, value = valueType)
          }
          case _ => throw new Exception("Unknown unary operator: " + op)
        }
      }
      case LookupExpr(id, index) => {
        val idType = typeCheckExpression(mapping, id)
        val indexType = typeCheckExpression(mapping, index)
        val infFlowType = idType.joinInfFlow(indexType)
        val valueType = idType.value
        new HyperTypeCollection(informationFlow = infFlowType, value = valueType)
      }
      case _ => {
        throw new Exception("Type error: cannot yet type check expression " + e)
      }
     }
  }
}