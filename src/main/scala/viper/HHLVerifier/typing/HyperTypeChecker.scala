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
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.PVarDecl
import viper.HHLVerifier.ast.MultiAssignStmt
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast.HyperAssumeStmt
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.dsl.TypeSystem
import viper.HHLVerifier.typing.dsl.StmtPattern
import viper.HHLVerifier.typing.dsl.CompStmt
import viper.HHLVerifier.ast.DeclareStmt
import viper.HHLVerifier.ast.ReuseStmt
import viper.HHLVerifier.ast.AssertStmt
import viper.HHLVerifier.ast.ProofVarDecl
import viper.HHLVerifier.ast.MethodCallStmt
import viper.HHLVerifier.ast.FrameStmt
import viper.HHLVerifier.ast.AssumeStmt
import viper.HHLVerifier.ast.UseHintStmt
import viper.HHLVerifier.typing.dsl.StatementDerivationResult
import viper.HHLVerifier.typing.dsl.InitStmt
import viper.HHLVerifier.generation.Generator.InvariantTracking.get
import viper.HHLVerifier.typing.dsl.MethodInitStmt

object HyperTypeChecker {

  val declaredVariables: Map[String, HyperType] = Map()

  var program: HHLProgram = HHLProgram(Seq.empty)

  var method: Option[Method] = None

  def typeCheckProg(system: TypeSystem, p: HHLProgram): Unit = {
    program = p
    program.content.foreach(m => { this.method = Some(m); typeCheckMethod(system, m) })
  }

  def typeCheckMethod(system: TypeSystem, m: Method): Unit = {

    val pc = new HyperTypeCollection(Set())

    val init            = system.initializeMethod(m)
    val typecheckResult = system.deriveStatement(init.hyperTypeMapping, init.deltaMapping, m.body, pc)

    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      val retType         = typecheckResult.hyperTypeMapping.get(r.name)
      if (!declaredRetType.isSubTypeOf(retType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
    })
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
  def getVariables(stmt: StmtPattern): Set[Id] = {
    stmt match {
      case CompStmt(s1, s2)                         => Set(s1, s2)
      case dsl.AssignStmt(left, right)              => Set(left, right)
      case dsl.IfStmt(cond, thenBranch, elseBranch) => Set(cond, thenBranch, elseBranch)
      case InitStmt()                       => Set.empty
      case dsl.HavocStmt(variable)                  => Set(variable)
      case MethodInitStmt(variable)                  => Set(variable)
    }
  }

  def getParameter(method: Method) : Set[Id] = {
    method.params.toSet
  }

  def getVariables(method: Method): Set[Id] = {
    getVariables(method.body) ++ method.params.toSet ++ method.res.toSet
  }

  def getVariables(stmt: Stmt): Set[Id] = {
    stmt match {
      case AssignStmt(left, right)          => Set(left) ++ getVariables(right)
      case AssertStmt(e)                    => getVariables(e)
      case MethodCallStmt(methodName, args) => args.toSet
      case MultiAssignStmt(left, right)     =>
        left.toSet ++ getVariables(right)
      case IfElseStmt(cond, ifStmt, elseStmt) =>
        getVariables(cond) ++ getVariables(ifStmt) ++ getVariables(elseStmt)
      case HavocStmt(id, hintDecl) => Set(id)
      case CompositeStmt(stmts)    =>
        stmts.flatMap(getVariables).toSet
      case PVarDecl(variable, _)                                                                                                                              => Set(variable)
      case WhileLoopStmt(cond, body, inv, decr, rule)                                                                                                         => getVariables(cond) ++ getVariables(body)
      case FoldStmt(t, id)                                                                                                                                    => Set(id)
      case UnfoldStmt(_, id)                                                                                                                                  => Set(id)
      case HyperAssumeStmt(_) | AssumeStmt(_) | UseHintStmt(_) | HyperAssertStmt(_) | ProofVarDecl(_, _) | DeclareStmt(_, _) | FrameStmt(_, _) | ReuseStmt(_) => throw new Exception("Statement type not supported for variable extraction: " + stmt.getClass.getSimpleName)
    }
  }

  def getAssignedVariables(stmt: Stmt): Set[Id] = {
    stmt match {
      case CompositeStmt(stmts)                       => stmts.flatMap(getAssignedVariables).toSet
      case AssignStmt(left, right)                    => Set(left)
      case MultiAssignStmt(left, right)               => left.toSet
      case HavocStmt(id, hintDecl)                    => Set(id)
      case IfElseStmt(cond, ifStmt, elseStmt)         => getAssignedVariables(ifStmt) ++ getAssignedVariables(elseStmt)
      case WhileLoopStmt(cond, body, inv, decr, rule) => getAssignedVariables(body)
      case _                                          => Set.empty
    }
  }
}
