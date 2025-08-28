package viper.HHLVerifier.typing

import viper.HHLVerifier.ast._
import viper.HHLVerifier.typing.dsl.{TypeSystem, HyperTypeCollection}
import viper.HHLVerifier.typing.dsl.ast.{StmtPattern, CompStmt => CompStmtPattern, InitStmt => InitStmtPattern, MethodInitStmt => MethodInitStmtPattern, AssignStmt => AssignStmtPattern, IfStmt => IfStmtPattern, HavocStmt => HavocStmtPattern}

/** HyperTypeChecker - given a `HHLProgram` and a `typeSystem` it will check the program of the hypertype correctness. This checker does not check the basic types and neither performs any symbol checking.
  */
object HyperTypeChecker {

  /** Type checks a HHL program
    * @param system
    *   the type system to use for type checking
    * @param program
    *   the program to type check
    * @throws Exception
    *   if the program is not well-typed
    */
  def typeCheckProg(system: TypeSystem, program: HHLProgram): Unit = {
    program.content.foreach(m => typeCheckMethod(system, m))
  }

  def typeCheckMethod(system: TypeSystem, m: Method): Unit = {

    val init            = system.initializeMethod(m)
    val typecheckResult = system.deriveStatement(init.hyperTypeMapping, init.deltaMapping, m.body)

    m.res.foreach(r => {
      val declaredRetType = HyperTypeCollection.fromSeq(r.hyperType.getOrElse(Seq()))
      val retType         = typecheckResult.hyperTypeMapping.get(r.name)
      if (!declaredRetType.isSubTypeOf(retType)) {
        throw new Exception("Type error: return type " + retType + " does not match declared type " + declaredRetType)
      }
    })
  }

  /** Finds all variables from an expression
    *
    * @param expr
    *   The expression to extract variables from
    * @return
    *   The set with all Id
    */
  def getVariables(expr: Expr): Set[Id] = {
    expr match {
      case Id(name)                 => Set(Id(name))
      case BinaryExpr(e1, _, e2)    => getVariables(e1) ++ getVariables(e2)
      case UnaryExpr(_, e)          => getVariables(e)
      case LookupExpr(id, index)    => getVariables(id) ++ getVariables(index)
      case LengthExpr(id)           => getVariables(id)
      case ImpliesExpr(left, right) => getVariables(left) ++ getVariables(right)
      case MethodCallExpr(_, args)  => args.toSet
      case MapAssignExpr(_)         => throw new Exception("MapAssignExpr not supported in getVariables")
      case MapTupleExpr(_, _)       => throw new Exception("MapTupleExpr not supported in getVariables")
      case SeqAssignExpr(_)         => throw new Exception("SeqAssignExpr not supported in getVariables")
      case SetAssignExpr(_)         => throw new Exception("SetAssignExpr not supported in getVariables")
      case _                        => Set.empty[Id]
    }
  }

  /** Finds all variables from a statement pattern
    *
    * @param stmt
    *   The statement pattern to extract variables from
    * @return
    *   The set with all Id
    */
  def getVariables(stmt: StmtPattern): Set[Id] = {
    stmt match {
      case CompStmtPattern(s1, s2)                     => Set(s1, s2)
      case AssignStmtPattern(left, right)              => Set(left, right)
      case IfStmtPattern(cond, thenBranch, elseBranch) => Set(cond, thenBranch, elseBranch)
      case InitStmtPattern()                           => Set.empty
      case HavocStmtPattern(variable)                  => Set(variable)
      case MethodInitStmtPattern(variable)             => Set(variable)
    }
  }

  /** Finds all variables from a statement. Only program variables are considered i.e. variables appearing in a `AssumeStmt` are ignored
    *
    * @param stmt
    *   The statement to extract variables from
    * @return
    *   The set with all Id
    */
  def getVariables(stmt: Stmt): Set[Id] = {
    stmt match {
      case AssignStmt(left, right)      => Set(left) ++ getVariables(right)
      case AssertStmt(e)                => getVariables(e)
      case MethodCallStmt(_, args)      => args.toSet
      case MultiAssignStmt(left, right) =>
        left.toSet ++ getVariables(right)
      case IfElseStmt(cond, ifStmt, elseStmt) =>
        getVariables(cond) ++ getVariables(ifStmt) ++ getVariables(elseStmt)
      case HavocStmt(id, _)     => Set(id)
      case CompositeStmt(stmts) =>
        stmts.flatMap(getVariables).toSet
      case PVarDecl(variable, _)                                                                                                                              => Set(variable)
      case WhileLoopStmt(cond, body, _, _, _)                                                                                                                 => getVariables(cond) ++ getVariables(body)
      case FoldStmt(_, id)                                                                                                                                    => Set(id)
      case UnfoldStmt(_, id)                                                                                                                                  => Set(id)
      case HyperAssumeStmt(_) | AssumeStmt(_) | UseHintStmt(_) | HyperAssertStmt(_) | ProofVarDecl(_, _) | DeclareStmt(_, _) | FrameStmt(_, _) | ReuseStmt(_) => Set.empty
    }
  }

  /** Finds all variables from a method.
    *
    * @param method
    *   The method to extract variables from
    * @return
    *   The set with all Id
    */
  def getVariables(method: Method): Set[Id] = {
    getVariables(method.body) ++ method.params.toSet ++ method.res.toSet
  }

  /** Finds all parameters from a method. The return values are not collected.
    *
    * @param method
    *   The method to extract parameters from
    * @return
    *   The set with all Id
    */
  def getParameter(method: Method): Set[Id] = {
    method.params.toSet
  }

  /** Finds all variables which are assigned in a statement. Only program variables are considered i.e. variables appearing in a `AssumeStmt` are ignored
    *
    * @param stmt
    *   The statement to extract assigned variables from
    * @return
    *   The set with all Id
    */
  def getAssignedVariables(stmt: Stmt): Set[Id] = {
    stmt match {
      case CompositeStmt(stmts)            => stmts.flatMap(getAssignedVariables).toSet
      case AssignStmt(left, _)             => Set(left)
      case MultiAssignStmt(left, _)        => left.toSet
      case HavocStmt(id, _)                => Set(id)
      case IfElseStmt(_, ifStmt, elseStmt) => getAssignedVariables(ifStmt) ++ getAssignedVariables(elseStmt)
      case WhileLoopStmt(_, body, _, _, _) => getAssignedVariables(body)
      case _                               => Set.empty
    }
  }
}
