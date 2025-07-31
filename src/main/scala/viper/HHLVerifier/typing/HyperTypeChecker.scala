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
    val pc = new HyperTypeCollection(Set(Low()))

    val mapping = m.params.map(p => (p.name -> {
      val baseCollection = HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq()))
      baseCollection.add(MonoUp(Set(Id(p.name))))
    })).toMap
    val hyperMapping = new HyperMapping(mapping)

    println("mapping at method start\n", hyperMapping)

    val (finalMapping, deltaMapping) = typeCheckStmt(hyperMapping, DeltaMapping(Map()), m.body, pc)
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

  def typeCheckStmt(mapping:  HyperMapping, delta: DeltaMapping, s : Stmt, pc: HyperTypeCollection) : (HyperMapping, DeltaMapping) = {
    s match {
      case AssignStmt(left, right) => {      
        return (mapping, delta)
      }
      case MultiAssignStmt(left, right) => {
        return (mapping, delta)
      }
      case CompositeStmt(stmts) => {
        val res =  stmts.foldLeft((mapping, delta))((acc, stmt) => {
          val (currentMapping, delta) = acc
          val (newMapping, newDelta) = typeCheckStmt(currentMapping, DeltaMapping(Map.empty), stmt, pc)
          (mapping, delta)
          })
        return res
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val (condType, _) = typeCheckExpression(mapping, cond)
        (mapping, delta)
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
        (mapping, delta)
      }

      case HavocStmt(Id(name), _) => {
        return (mapping, delta)
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

  def typeCheckMethodExpr(mapping: HyperMapping, delta : DeltaCollection, e: MethodCallExpr) : Seq[HyperTypeCollection] = {
    val method = program.methods.find(_.mName == e.methodName) match {
      case None => throw new Exception("Method not found: " + e.methodName + " in " + program.methods.map(_.mName).mkString(", "))
      case Some(value) => value
    }
    val methodArgTypes = method.params.map(p => HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq())))
   
    for ((name, ty) <- e.args.zip(methodArgTypes)) {
      val (actual_type, _) = typeCheckExpression(mapping, name)
      if (!actual_type.isSubTypeOf(ty)) {
        throw new Exception("Type error: argument " + name + " of type " + actual_type + " does not match expected type " + ty)
      }
    }
    method.res.map(r => HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq())))
  }


  def typeCheckExpression(mapping: HyperMapping, e: Expr) : (HyperTypeCollection, DeltaCollection) = {
    e match {
      case BoolLit(b) => {
        val value = if (b) True() else False()
        (new HyperTypeCollection(Set(Low(), value)), DeltaCollection(Map()))
      }
      case Num(n) => {
        val value = if (n > 0) Pos() else if (n < 0) Neg() else Zero()
        val absValue = if (n < 1 && n > -1) LessOne() else if (n==1 || n == -1) One() else GreaterOne()
        (new HyperTypeCollection(Set(Low(), value, absValue)), DeltaCollection(Map()))
      }
      case Id(name) => {
        (mapping.getUnsafe(name), DeltaCollection(Map()))
      }
      case BinaryExpr(e1, op, e2) => 
        {
          val (type1, delta1) = typeCheckExpression(mapping, e1)
          (type1, DeltaCollection(Map()))
        }

      case LengthExpr(id) => {
        typeCheckExpression(mapping, id)
      }
      case UnaryExpr(op, e) => {
        op match {
          case "-" => {
            return typeCheckExpression(mapping, BinaryExpr(Num(0), "-", e))
          }
          case "!" => {
            // Negation of boolean
            return typeCheckExpression(mapping, BinaryExpr(e, "==", BoolLit(false)))
          }
          case _ => throw new Exception("Unknown unary operator: " + op)
        }
      }
      case LookupExpr(id, index) => {
        val (idType, _) = typeCheckExpression(mapping, id)
        val (indexType, _) = typeCheckExpression(mapping, index)
        (idType, new DeltaCollection(Map()))
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