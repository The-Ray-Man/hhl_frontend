package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl.utils.{SpecificationUtil, Substitution}
import viper.HHLVerifier.typing.dsl.ast.{HyperTypeDeclaration, ExpressionDerivationRule, MethodInitStmt, AssignStmt, CompStmt, IfStmt, Specification, StatementDerivationRule, Rule}
import viper.HHLVerifier.typing.dsl
import viper.HHLVerifier.typing.dsl.ast.{HavocStmt => HavocStmtPattern, StmtPattern}
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.dsl.ExpressionDerivationResult
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.FrameStmt
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast
import viper.HHLVerifier.ast.DeclareStmt
import viper.HHLVerifier.ast.ProofVarDecl
import viper.HHLVerifier.ast.MultiAssignStmt
import viper.HHLVerifier.ast.MethodCallStmt
import viper.HHLVerifier.ast.UseHintStmt
import viper.HHLVerifier.ast.WhileLoopStmt
import viper.HHLVerifier.ast.HyperAssumeStmt
import viper.HHLVerifier.ast.PVarDecl
import viper.HHLVerifier.ast.AssumeStmt
import viper.HHLVerifier.ast.IfElseStmt
import viper.HHLVerifier.ast.HavocStmt
import viper.HHLVerifier.ast.ReuseStmt
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.UnfoldStmt
import viper.HHLVerifier.ast.FoldStmt
import viper.HHLVerifier.ast.AssertStmt
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getParameter
import viper.HHLVerifier.typing.dsl.utils.Cache

/** Type system for the DSL
  */
case class TypeSystem(
    statementTypeSystem: StatementTypeSystem,
    expressionTypeSystem: Seq[ExpressionDerivationRule],
    hyperTypeDeclaration: Seq[HyperTypeDeclaration] = Seq.empty[HyperTypeDeclaration]
) {

  var allVariables: ScalaSet[Id] = Set.empty
  var allParams: ScalaSet[Id]    = Set.empty

  /** This function executes the `init` rule. The function accepts a `gamma` and `delta` because they may exists when one wants to initialize for a if-else branch.
    * @param gamma
    *   The hypermapping for the current context.
    * @param delta
    *   The deltamapping for the current context.
    * @return
    *   StatementDerivationResult containing the derived hypermapping and deltamapping.
    */
  def init(gamma: HyperMapping, delta: DeltaMapping): StatementDerivationResult = {
    val context = StatementDerivationContext(this, null, allVariables, gamma, delta, Map.empty, Map.empty, new Cache())
    statementTypeSystem.initRule.derive(context)
  }

  /** This function executes the `methodInit` rule, given in the DSL to all variables.
    * @param method
    *   The method to initialize.
    * @return
    *   StatementDerivationResult containing the derived hypermapping and deltamapping.
    */
  def initializeMethod(method: ast.Method): StatementDerivationResult = {
    allVariables = getVariables(method)
    allParams = getParameter(method)

    val result = allVariables
      .map(v => {
        val emptyContext = StatementDerivationContext(
          typeSystem = this,
          stmt = null,
          allVars = allVariables,
          gamma = new HyperMapping(Map.empty),
          delta = DeltaMapping(Map.empty),
          varStmtMapping = Map.empty,
          varExprMapping = Map(statementTypeSystem.methodInitRule.statement.asInstanceOf[MethodInitStmt].variable -> v),
          cache = new Cache()
        )
        val res            = statementTypeSystem.methodInitRule.derive(emptyContext)
        val htypesFromDecl = v.hyperType.getOrElse(Seq()).toSet
        val htypesFromInit = res.hyperTypeMapping.get(v.name).hypertypes
        val deltaTypes     = res.deltaMapping.collection.getOrElse(v.name, DeltaCollection(Map()))
        (v.name -> HyperTypeCollection(htypesFromDecl ++ htypesFromInit), (v.name -> deltaTypes))
      })
      .toSeq

    val hyperTypeMapping = HyperMapping(result.map(_._1).toMap)
    val deltaMapping     = DeltaMapping(result.map(_._2).toMap)

    val hyperTypeMappingWithDecl = method.params.foldLeft(hyperTypeMapping) { case (agg, param) =>
      val declaredTypes = param.hyperType.getOrElse(Seq()).toSet
      val currentTypes  = agg.mapping.getOrElse(param.name, HyperTypeCollection(Set()))
      agg.copy(mapping = agg.mapping.updated(param.name, HyperTypeCollection(currentTypes.hypertypes ++ declaredTypes)))
    }

    StatementDerivationResult(
      hyperTypeMapping = hyperTypeMappingWithDecl,
      deltaMapping = deltaMapping
    )
  }

  /** This function derives the type information for a given expression.
    * @param gamma
    *   The hypermapping for the current context.
    * @param delta
    *   The deltamapping for the current context.
    * @param expr
    *   The expression to derive the type information for.
    *
    * @return
    *   ExpressionDerivationResult containing the derived hypercollection and deltacollection.
    */
  def deriveExpression(gamma: HyperMapping, delta: DeltaMapping, expr: Expr): ExpressionDerivationResult = {
    println("start with:", expr)
    if (expr.isInstanceOf[ast.MethodCallExpr]) {
      val results = deriveMethodCallExpr(gamma, delta, expr.asInstanceOf[ast.MethodCallExpr])
      if (results.size == 0) {
        return ExpressionDerivationResult(
          hyperTypeCollection = HyperTypeCollection(Set()),
          deltaCollection = DeltaCollection(Map())
        )
      } else if (results.size == 1) {
        return results.head
      } else {
        throw new Exception("Treating a method call which returns multiple values as a normal expression is currently not supported.")
      }
    }

    val applicableRules = expressionTypeSystem.map(rule => (rule, rule.isApplicableTo(expr))).filter(_._2.isDefined).map(rule => (rule._1, rule._2.get))
    if (applicableRules.isEmpty || applicableRules.length > 1) {
      throw new Exception(s"There are ${applicableRules.length} applicable rules for expression $expr")
    }
    val rule   = applicableRules.head
    val result = rule._1.derive(this, gamma, delta, expr, rule._2)
    println(s"{${gamma.toString()}} {${delta.toString()}} |- ${expr.toString()} :: {${result.hyperTypeCollection.toString()}} {${result.deltaCollection.toString()}} | ${result.appliedRules.mkString(", ")}")
    result
  }

  /** Derives the type information for a method call expression. For every parameter of the function call it is checked if the variable has the right types.
    *
    * @param gamma
    *   The hypermapping for the current context.
    * @param delta
    *   The deltamapping for the current context.
    * @param expr
    *   The method call expression to derive the type information for.
    * @return
    *   A sequence of ExpressionDerivationResult containing the derived hypercollection and deltacollection.
    */
  def deriveMethodCallExpr(gamma: HyperMapping, delta: DeltaMapping, expr: ast.MethodCallExpr): Seq[ExpressionDerivationResult] = {
    val argsCorrect = expr.args.zip(expr.method.params).forall { case (arg, param) =>
      val argHyperType   = gamma.get(arg.name)
      val paramHyperType = HyperTypeCollection(param.hyperType.getOrElse(Seq()).toSet)
      paramHyperType.isSubTypeOf(argHyperType)
    }
    // Some hypertypes are related to method parameters. Hence we need to rename them
    val renameMapping = expr.method.params.zip(expr.args).toMap

    if (argsCorrect) {
      val returnTypes = expr.method.res.map(id => HyperTypeCollection(id.hyperType.getOrElse(Seq()).toSet))
      returnTypes.map(rt =>
        ExpressionDerivationResult(
          hyperTypeCollection = Substitution.apply(renameMapping, rt),
          deltaCollection = DeltaCollection(Map())
        )
      )
    } else {
      throw new Exception(s"Method call ${expr} has arguments that do not match the declared types in gamma: ${gamma}")
    }
  }

  /** Derives the type information for a statement.
    * @param gamma
    *   The hypermapping for the current context.
    * @param delta
    *   The deltamapping for the current context.
    * @param s
    *   The statement to derive the type information for.
    * @return
    *   A StatementDerivationResult containing the derived hypermapping and deltamapping.
    */

  def deriveStatement(gamma: HyperMapping, delta: DeltaMapping, s: Stmt): StatementDerivationResult = {
    println("start with: ", s)
    val res = s match {
      case ast.AssignStmt(left, right) => {
        val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.assignRule.statement).getOrElse(throw new Exception(s"Statement $s does not match assign pattern"))
        val context                      = StatementDerivationContext(this, s, allVariables, gamma, delta, stmtMatching, exprMatching, new Cache())
        statementTypeSystem.assignRule.derive(context)
      }
      case CompositeStmt(stmts) => {
        if (stmts.length == 0) {
          StatementDerivationResult(gamma, delta)
        } else if (stmts.length == 1) {
          deriveStatement(gamma, delta, stmts.head)
        } else {
          val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.compositionRule.statement).getOrElse(throw new Exception(s"Statement $s does not match assign pattern"))
          val context                      = StatementDerivationContext(this, s, allVariables, gamma, delta, stmtMatching, exprMatching, new Cache())
          statementTypeSystem.compositionRule.derive(context)
        }
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.branchRule.statement).getOrElse(throw new Exception(s"Statement $s does not match branch pattern"))
        val context                      = StatementDerivationContext(this, s, allVariables, gamma, delta, stmtMatching, exprMatching, new Cache())
        val derivedResult                = statementTypeSystem.branchRule.derive(context)
        derivedResult
      }
      case WhileLoopStmt(cond, body, _, _, _) => {
        var currentGamma = gamma
        var currentDelta = delta
        var newGamma     = gamma
        var newDelta     = delta
        do {
          currentGamma = newGamma
          currentDelta = newDelta
          val condResult = deriveStatement(currentGamma, currentDelta, IfElseStmt(cond, body, CompositeStmt(Seq())))
          newGamma = condResult.getStatementResult.hyperTypeMapping
          newDelta = condResult.getStatementResult.deltaMapping
        } while (newGamma != currentGamma || newDelta != currentDelta)
        StatementDerivationResult(
          hyperTypeMapping = newGamma,
          deltaMapping = newDelta
        )
      }
      case UnfoldStmt(htyp, id) => {
        val typeCollection = gamma.get(id.name)
        if (!typeCollection.hypertypes.contains(htyp)) {
          throw new Exception(s"${id} may not have hyper type ${htyp}")
        } else {
          StatementDerivationResult(
            hyperTypeMapping = gamma,
            deltaMapping = delta
          )
        }
      }
      case FoldStmt(htyp, id) => {
        StatementDerivationResult(
          hyperTypeMapping = HyperMapping(gamma.mapping.updated(id.name, gamma.mapping.getOrElse(id.name, HyperTypeCollection(Set())).add(htyp))),
          deltaMapping = delta
        )
      }
      case HavocStmt(id, _) => {
        val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.havocRule.statement).getOrElse(throw new Exception(s"Statement $s does not match havoc pattern"))
        val context                      = StatementDerivationContext(this, s, allVariables, gamma, delta, stmtMatching, exprMatching, new Cache())
        val derivedResult                = statementTypeSystem.havocRule.derive(context)
        derivedResult
      }
      case MultiAssignStmt(ids, expr) => {
        if (ids.size != 1) {
          throw new Exception("Method calls with not exactly one return value are not supported yet.")
        }
        deriveStatement(gamma, delta, ast.AssignStmt(ids.head, expr))
      }
      case MethodCallStmt(_, _)                                                                                                                                                                => throw new Exception("Method calls are not yet supported in the type system")
      case AssumeStmt(_) | UseHintStmt(_) | HyperAssumeStmt(_) | PVarDecl(_, _) | DeclareStmt(_, _) | HyperAssertStmt(_) | ProofVarDecl(_, _) | ReuseStmt(_) | AssertStmt(_) | FrameStmt(_, _) => StatementDerivationResult(gamma, delta)
    }
    println(s"{${gamma.toString()}} {${delta.toString()}}} |- ${s.toString()} :: {${res.hyperTypeMapping.toString()}} {${res.deltaMapping.toString()}}| ${res.appliedRules.mkString(", ")}")
    res
  }

  /** Checks if a statement matches a given pattern. If it matches a mapping is returned.
    *
    * @param stmt
    *   The statement to check.
    * @param pattern
    *   The pattern to match against.
    * @return
    *   if the statement matches, a mapping of IDs to statements and another mapping from IDs to Expr are returned
    */
  def statementMatchesPattern(stmt: Stmt, pattern: StmtPattern): Option[(Map[Id, Stmt], Map[Id, Expr])] = {
    (pattern, stmt) match {
      case (AssignStmt(left, right), ast.AssignStmt(leftCheck, rightCheck)) =>
        Some((Map.empty, Map(left -> leftCheck, right -> rightCheck)))
      case (IfStmt(cond, ifStmt, elseStmt), IfElseStmt(condCheck, ifStmtCheck, elseStmtCheck)) =>
        Some(Map(ifStmt -> ifStmtCheck, elseStmt -> elseStmtCheck), Map(cond -> condCheck))
      case (CompStmt(s1, s2), CompositeStmt(stmtsCheck)) => {
        if (stmtsCheck.length == 1) {
          throw new Exception("CompositeStmt pattern matching is not supported for single statements")
        } else if (stmtsCheck.length == 2) {
          Some(Map(s1 -> stmtsCheck.head, s2 -> stmtsCheck(1)), Map.empty)
        } else {
          Some(Map(s1 -> stmtsCheck.head, s2 -> CompositeStmt(stmtsCheck.tail)), Map.empty)
        }
      }
      case (HavocStmtPattern(variable), HavocStmt(id, _)) => Some((Map.empty, Map(variable -> id)))
      case _                                              => None
    }
  }
}

/** Wrapper for statement type system rules.
  */
case class StatementTypeSystem(assignRule: StatementDerivationRule, compositionRule: StatementDerivationRule, branchRule: StatementDerivationRule, initRule: StatementDerivationRule, methodInitRule: StatementDerivationRule, havocRule: StatementDerivationRule) {}

object TypeSystem {

  /** Reads the type system from the specified paths.
    *
    * @param paths
    *   The paths to the type system files.
    * @return
    *   The loaded type system.
    */
  def loadTypeSystem(paths: Seq[String]): TypeSystem = {
    val specifications = paths.map(path => {
      val fileContent = scala.io.Source.fromFile(path).getLines().mkString("\n")
      val res         = fastparse.parse(fileContent, viper.HHLVerifier.typing.dsl.Parser.specification(_))
      res match {
        case fastparse.Parsed.Success(value, _) => nameRules(value, path)
        case failure: fastparse.Parsed.Failure  => throw new Exception(s"Failed to parse type system: ${failure.msg}")
      }
    })

    SpecificationUtil.combineSpecifications(specifications).toTypeSystem()
  }

  /** Gives every rule a name for tracking purposes. The name consists of the filename, the rule and the index of the rule.
    */
  def nameRules(spec: Specification, filepath: String): Specification = {

    spec.copy(derivationRules =
      spec.derivationRules.map(rule =>
        rule match {
          case ExpressionDerivationRule(expr, rules)     => ExpressionDerivationRule(expr, rules.zipWithIndex.map { case (Rule(cond, conc, _), index) => Rule(cond, conc, RuleName.create(filepath, rule, index)) })
          case StatementDerivationRule(statement, rules) => StatementDerivationRule(statement, rules.zipWithIndex.map { case (Rule(cond, conc, _), index) => Rule(cond, conc, RuleName.create(filepath, rule, index)) })
        }
      )
    )
  }
}
