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
import viper.silver.plugin.standard.adt.PAdtOpApp.typecheck

object HyperTypeChecker {

  val declaredVariables: Map[String, HyperType] = Map()

  val program : HHLProgram = HHLProgram(Seq.empty)


  def typeCheckProg(p: HHLProgram): Unit = {
        p.content.foreach(m => typeCheckMethod(m))
  }

  def typeCheckMethod(m: Method): Unit = {
    // Type check the method body
    val pc = new HyperTypeCollection(Some(Low()))

    val mapping = m.params.map(p => (p.name -> HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq())))).toMap
    val hyperMapping = new HyperMapping()
    hyperMapping.mapping = mapping

    val finalMapping = typeCheckStmt(hyperMapping, m.body, pc)
    finalMapping.mapping.foreach({case (name, value) =>
      // println(s"Variable: $name, Type: $value")
  })

    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      val retType = finalMapping.getUnsafe(r.name)
      // println(s"Dseclared return type: $declaredRetType, Actual return type: $retType")
      if (!HyperLattice.lteq(retType, declaredRetType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
    })
  }

  def typeCheckStmt(mapping: HyperMapping, s : Stmt, pc: HyperTypeCollection) : HyperMapping = {
    s match {
      case AssignStmt(left, right) => {
         val rightHyperType = typeCheckExpression(mapping, right)
         val hyperType = HyperLattice.join(rightHyperType, pc)
         mapping.set(left.name, hyperType)
         mapping
      }
      case CompositeStmt(stmts) => {
        stmts.foldLeft(mapping)((acc, stmt) => typeCheckStmt(acc, stmt, pc))
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val condType = typeCheckExpression(mapping, cond)
        val new_pc = HyperLattice.join(condType, pc)
        val mappingIf = typeCheckStmt(mapping, ifStmt, new_pc)
        val mappingElse = typeCheckStmt(mapping, elseStmt, new_pc)
        mappingIf.join(mappingElse)
      }
      case UnfoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val var_type = mapping.getUnsafe(id.name)
        if (HyperLattice.lteq(var_type, ty)) {
          mapping
        } else {
          throw new Exception("Type error: cannot unfold " + id.name + " of type " + var_type + " to type " + t)
        }
        mapping
      }
      case FoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        mapping.set(id.name, ty)
        mapping
      }
      case WhileLoopStmt(cond, body, _, _, _) => {
        val previous_mapping = mapping
        var new_mapping = mapping
        do {
          val condType = typeCheckExpression(mapping, cond)
          val new_pc = HyperLattice.join(condType, pc)
          new_mapping = typeCheckStmt(mapping, body, new_pc)
        } while (new_mapping != previous_mapping)
          previous_mapping
        }
      case HavocStmt(id, _) => {
        mapping.set(id.name, HyperLattice.maximum())
        mapping
      }
      case PVarDecl(_, _) => {
        mapping
      }

      case _ => {
        throw new Exception("Type error: cannot yet type check statement " + s)
      }
    }
  }


  def typeCheckExpression(mapping: HyperMapping, e: Expr) : HyperTypeCollection = {
    e match {
      case BoolLit(_) => {
        new HyperTypeCollection(Some(Low()))
      }
      case Num(_) => {
        new HyperTypeCollection(Some(Low()))
      }
      case Id(name) => {
        mapping.getUnsafe(name)
      }
      case BinaryExpr(e1, _, e2) => 
        {
          val type1 = typeCheckExpression(mapping, e1)
          val type2 = typeCheckExpression(mapping, e2)
          HyperLattice.join(type1, type2)
        }

      case LengthExpr(id) => {
        typeCheckExpression(mapping, id)
      }
      case UnaryExpr(_, e) => {
        val type1 = typeCheckExpression(mapping, e)
        type1
      }
      case LookupExpr(id, index) => {
        val idType = typeCheckExpression(mapping, id)
        val indexType = typeCheckExpression(mapping, index)
        HyperLattice.join(idType, indexType)
      }
      // case MethodCallExpr(methodName, args) => {
      //   val method = program.methods.find(_.mName == methodName) match {
      //     case None => throw new Exception("Method not found: " + methodName)
      //     case Some(value) => value
      //   }
      //   val methodArgTypes = method.params.map(p => p.hyperType.get(0))
      //   val argTypes = args.map(arg => typeCheckExpression(mapping, arg))
      //   if (methodArgTypes.length != argTypes.length) {
      //     throw new Exception("Method " + methodName + " called with wrong number of arguments")
      //   }
      //   for (i <- 0 until methodArgTypes.length) {
      //     if (!HyperLattice.lteq(argTypes(i), thodArgTypes(i))) {
      //       throw new Exception("Method " + methodName + " called with wrong argument type: expected " + methodArgTypes(i) + " but got " + argTypes(i))
      //     }
      //   }
      //   method.res.map(_.hyperType.get(0))

      // }
      case _ => {
        throw new Exception("Type error: cannot yet type check expression " + e)
      }
     }
  }
}