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
    val hyperMapping = new HyperMapping()
    hyperMapping.mapping = mapping

    val finalMapping = typeCheckStmt(hyperMapping, m.body, pc)
    finalMapping.mapping.foreach({case (name, value) =>
      // println(s"Variable: $name, Type: $value")
  })
    println(finalMapping.mapping)
    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      if (!declaredRetType.is_empty()) {
        
      val retType = finalMapping.getUnsafe(r.name)
      // println(s"Dseclared return type: $declaredRetType, Actual return type: $retType")
      if (!HyperLattice.lteq(retType, declaredRetType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
      }
    })
  }

  def typeCheckStmt(mapping:  HyperMapping, s : Stmt, pc: HyperTypeCollection) : HyperMapping = {
    
    s match {
      case AssignStmt(left, right) => {
         val rightHyperType = typeCheckExpression(mapping, right)
         val hyperType = HyperLattice.join(rightHyperType, pc)
         val newMapping = mapping.set(left.name, hyperType)
         return newMapping
      }
      case MultiAssignStmt(left, right) => {
        val rightHyperType = typeCheckMethodExpr(mapping, right)
        var newMapping = mapping;
        for ((name, ty) <- left.zip(rightHyperType)) {
          val hyperType = HyperLattice.join(ty, pc)
          newMapping = newMapping.set(name.name, hyperType)
        }
        return newMapping
      }
      case CompositeStmt(stmts) => {
        return stmts.foldLeft(mapping)((acc, stmt) => typeCheckStmt(acc, stmt, pc))
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val condType = typeCheckExpression(mapping, cond)
        val new_pc = HyperLattice.join(condType, pc)
        val mappingIf = typeCheckStmt(mapping, ifStmt, new_pc)
        val mappingElse = typeCheckStmt(mapping, elseStmt, new_pc)
        val newMapping = mappingIf.join(mappingElse)
        return newMapping
      }
      case UnfoldStmt(t, id) => {
        val ty = HyperTypeCollection.fromSeq(Seq(t))
        val var_type = mapping.getUnsafe(id.name)
        if (HyperLattice.lteq(var_type, ty)) {
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
        val initial_mapping = mapping
        var previous_mapping = mapping
        var newMapping = mapping
        do {
          previous_mapping = newMapping
          val condType = typeCheckExpression(newMapping, cond)
          val new_pc = HyperLattice.join(condType, pc)
          newMapping = typeCheckStmt(newMapping, body, new_pc)
        } while (newMapping != previous_mapping)

        newMapping = previous_mapping.join(initial_mapping)
        return newMapping
        }
      case HavocStmt(id, _) => {
        val newMapping = mapping.set(id.name, HyperLattice.maximum())
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
      if (!HyperLattice.lteq(actual_type, ty)) {
        throw new Exception("Type error: argument " + name + " of type " + actual_type + " does not match expected type " + ty)
      }
    }
    method.res.map(r => HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq())))
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
      case _ => {
        throw new Exception("Type error: cannot yet type check expression " + e)
      }
     }
  }
}