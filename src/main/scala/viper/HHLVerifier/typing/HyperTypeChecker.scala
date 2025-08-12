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
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.rules.TypeSystem

object HyperTypeChecker {

  val declaredVariables: Map[String, HyperType] = Map()

  var program: HHLProgram = HHLProgram(Seq.empty)

  var method: Option[Method] = None

  def typeCheckProg(system: TypeSystem, p: HHLProgram): Unit = {
    program = p
    program.content.foreach(m => { this.method = Some(m); typeCheckMethod(system, m) })
  }

  def typeCheckMethod(system: TypeSystem, m: Method): Unit = {
    // Type check the method body
    val pc = new HyperTypeCollection(Set())

    val mapping = m.params
      .map(p =>
        (p.name -> {
          HyperTypeCollection.fromSeq(p.hyperType.getOrElse(Seq()))
        })
      )
      .toMap
    val hyperMapping = new HyperMapping(mapping)

    println("mapping at method start\n", hyperMapping)

    val (finalMapping, deltaMapping) = typeCheckStmt(system, hyperMapping, DeltaMapping(Map()), m.body, pc)
    println("final mapping", finalMapping.mapping)
    println("delta mapping")
    deltaMapping.collection.foreach { case (key, value) =>
      println(s"$key: ${value.mapping}")
    }
    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      val retType         = finalMapping.getUnsafe(r.name)
      if (!retType.isSubTypeOf(declaredRetType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
    })
  }

  def typeCheckStmt(system: TypeSystem, mapping: HyperMapping, delta: DeltaMapping, s: Stmt, pc: HyperTypeCollection): (HyperMapping, DeltaMapping) = {
    s match {
      case AssignStmt(left, right) => {
        return (mapping, delta)
      }
      case MultiAssignStmt(left, right) => {
        return (mapping, delta)
      }
      case CompositeStmt(stmts) => {
        val res = stmts.foldLeft((mapping, delta))((acc, stmt) => {
          val (currentMapping, delta) = acc
          val (newMapping, newDelta)  = typeCheckStmt(system, currentMapping, DeltaMapping(Map.empty), stmt, pc)
          (mapping, delta)
        })
        return res
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        // val (condType, _) = typeCheckExpression(system.expressionTypeSystem, mapping, cond)
        (mapping, delta)
      }
      case UnfoldStmt(t, id) => {
        val ty       = HyperTypeCollection.fromSeq(Seq(t))
        val var_type = mapping.getUnsafe(id.name)
        if (var_type.isSubTypeOf(ty)) {
          return (mapping, delta)
        } else {
          throw new Exception("Type error: cannot unfold " + id.name + " of type " + var_type + " to type " + t)
        }
      }
      case FoldStmt(t, id) => {
        val ty         = HyperTypeCollection.fromSeq(Seq(t))
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
        return (mapping, delta)
      }

      case _ => {
        throw new Exception("Type error: cannot yet type check statement " + s)
      }
    }
  }

  def typeCheckExpression(system: TypeSystem, gamma: HyperMapping, delta: DeltaMapping, e: Expr): (HyperTypeCollection, DeltaCollection) = {
    system.deriveExpression(gamma, delta, e, Map())
  }

  def getVariables(expr: Expr): Set[Id] = {
    expr match {
      case Id(name)              => Set(Id(name))
      case BinaryExpr(e1, _, e2) => getVariables(e1) ++ getVariables(e2)
      case UnaryExpr(_, e)       => getVariables(e)
      case LookupExpr(id, index) => getVariables(id) ++ getVariables(index)
      case LengthExpr(id)        => getVariables(id)
      case _                     => { Set.empty[Id] }
    }
  }
}
