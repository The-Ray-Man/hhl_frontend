package viper.HHLVerifier.typing.dsl.ast

import viper.HHLVerifier.typing
import scala.collection.immutable
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.LengthExpr
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.dsl.ast.Mapping
import viper.HHLVerifier.typing.dsl.{HyperMapping, DeltaMapping, HyperTypeCollection, DeltaCollection, CollectVariables}
import viper.HHLVerifier.typing.dsl.utils.{Cache, Substitution}
import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.typing.dsl.{TypeSystem, StatementTypeSystem, Element, HyperType, SimpleHyperType, HyperTypeWithListArgs, HyperTypeWithSetArgs, RuleWrapper, EmptyWrapper, ForanyVariableWrapper, StatementDerivationContext, StatementDerivationResult, ExpressionDerivationResult, ExpressionDerivationContext, RuleName}

/** A `Specification` is the abstract syntax tree returned by the parser.
  *
  * @param hypertypeDeclaration
  *   This contains the semantic declaration of the hypertypes.
  * @param derivationRules
  *   This contains the rules for deriving the types for expressions as well as statements.
  */
case class Specification(hypertypeDeclaration: Seq[HyperTypeDeclaration], derivationRules: Seq[DerivationRule]) {

  /** Converts the specification to a type system.
    *
    * @param mustHaveAllStmtRules
    *   if this is set to false, then the missing statement rules will just be set to null.
    * @return
    *   The typesystem defined by the specification.
    * @throws Exception
    *   - if a hypertype has multiple declarations (even if they are the same) or
    *   - if not all statement derivation rules are defined
    */
  def toTypeSystem(mustHaveAllStmtRules: Boolean = true): TypeSystem = {
    val expressionRules = derivationRules.filter(_.isInstanceOf[ExpressionDerivationRule]).map(rule => renameVariables(rule.asInstanceOf[ExpressionDerivationRule]))
    val statementRules  = derivationRules.filter(_.isInstanceOf[StatementDerivationRule]).map(rule => renameVariables(rule.asInstanceOf[StatementDerivationRule]))

    def handleFindResult(rules: Option[StatementDerivationRule]): StatementDerivationRule = {
      rules match {
        case Some(rule) => rule
        case None       => {
          if (mustHaveAllStmtRules) {
            throw new Exception(s"One stmt rule could not be found.")
          } else {
            null
          }
        }
      }
    }
    val typeSystem = TypeSystem(
      statementTypeSystem = StatementTypeSystem(
        assignRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[AssignStmt])),
        branchRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[IfStmt])),
        compositionRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[CompStmt])),
        initRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[InitStmt])),
        havocRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[HavocStmt])),
        methodInitRule = handleFindResult(statementRules.find(_.statement.isInstanceOf[MethodInitStmt]))
      ),
      expressionTypeSystem = expressionRules,
      hyperTypeDeclaration = hypertypeDeclaration
    )

    for ((htypeDecl, i) <- hypertypeDeclaration.zipWithIndex) {
      if (hypertypeDeclaration.zipWithIndex.find { case (decl, j) => i < j && structureEqual(decl.hty, htypeDecl.hty) }.isDefined) {
        throw new Exception("Duplicate hypertype declaration for type (" + htypeDecl.hty + ")")
      }
    }

    typeSystem
  }

  /** Renames the placeholder variables to unique names. This is used to avoid naming conflicts.
    *
    * @param rule
    * @return
    */
  def renameVariables(rule: ExpressionDerivationRule): ExpressionDerivationRule = {
    val allVariables     = HyperTypeChecker.getVariables(rule.expr) ++ rule.rules.flatMap(_.variables).toSet
    val renamedVariables = allVariables.map { case id => id -> Id(s"VAR'${id.name}'") }.toMap
    val substitution     = Substitution(renamedVariables)
    val renamedRules     = rule.rules.map(substitution.apply)
    val renamedExpr      = substitution.apply(rule.expr)
    ExpressionDerivationRule(renamedExpr, renamedRules)
  }

  /** Renames the placeholder variables to unique names. This is used to avoid naming conflicts.
    *
    * @param rule
    * @return
    */
  def renameVariables(rule: StatementDerivationRule): StatementDerivationRule = {
    val allVariables     = HyperTypeChecker.getVariables(rule.statement) ++ rule.rules.flatMap(_.variables).toSet
    val renamedVariables = allVariables.map { case id => id -> Id(s"VAR'${id.name}'") }.toMap
    val substitution     = Substitution(renamedVariables)
    val renamedRules     = rule.rules.map(substitution.apply)
    val renamedStmt      = substitution.apply(rule.statement)
    StatementDerivationRule(renamedStmt, renamedRules)
  }

  /** Checks if two `Element`s are structurally equal. Two elements are equal iff one can rename the placeholder variables such that they match exactly.
    */
  def structureEqual(htypFrom: Element, htypTo: Element): Boolean = {
    (htypFrom, htypTo) match {
      case (SimpleHyperType(fromName), SimpleHyperType(toName))                               => fromName == toName
      case (HyperTypeWithListArgs(fromName, fromArgs), HyperTypeWithListArgs(toName, toArgs)) => fromName == toName && fromArgs.length == toArgs.length && fromArgs.zip(toArgs).forall(pair => structureEqual(pair._1, pair._2))
      case (HyperTypeWithSetArgs(fromName, fromArgs), HyperTypeWithSetArgs(toName, toArgs))   => fromName == toName && fromArgs.size == toArgs.size && fromArgs.forall(fArg => toArgs.exists(tArg => structureEqual(fArg, tArg)))
      case (Id(_), Id(_))                                                                     => true
      case _                                                                                  => false
    }
  }
}

/** Contains a hypertype declaration.
  *
  * @param variable
  *   The variable used as a placeholder.
  * @param hty
  *   The hypertype one wants to declare.
  * @param definition
  *   The definition of the hypertype.
  */
case class HyperTypeDeclaration(variable: Id, hty: HyperType, definition: Expr) {}

trait DerivationRule

case class ExpressionDerivationRule(expr: Expr, rules: Seq[Rule]) extends DerivationRule {

  val wrappedRules: Seq[RuleWrapper] = rules.map(rule => wrapRule(rule))

  def isApplicableTo(toCheckExpression: Expr): Option[Map[Id, Expr]] = {
    // Check if the expression matches the rule's expression
    // Returns a mapping that maps the "template ids" to the Id in the expression.
    (expr, toCheckExpression) match {
      case (BinaryExpr(left, op, right), BinaryExpr(leftCheck, opCheck, rightCheck)) if op == opCheck => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (UnaryExpr(op, inner), UnaryExpr(opCheck, innerCheck)) if op == opCheck                    => Some(matchHelper(Seq((inner, innerCheck))))
      case (id @ Id("VAR'n'"), Num(value))                                                            => Some(Map(id -> Num(value)))
      case (id @ Id("VAR'var'"), Id(name))                                                            => Some(Map(id -> Id(name)))
      case (id @ Id("VAR'b'"), BoolLit(name))                                                         => Some(Map(id -> BoolLit(name)))
      case (ImpliesExpr(left, right), ImpliesExpr(leftCheck, rightCheck))                             => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (_, MethodCallExpr(_, _))                                                                  => throw new Exception("Method calls are not supported in expression derivation rules")
      case (LookupExpr(dataStructure, index), LookupExpr(dataStructureCheck, indexCheck))             => Some(matchHelper(Seq((dataStructure, dataStructureCheck), (index, indexCheck))))
      case (LengthExpr(dataStructure), LengthExpr(dataStructureCheck))                                => Some(matchHelper(Seq((dataStructure, dataStructureCheck))))
      case _                                                                                          => None
    }
  }

  def matchHelper(matching: Seq[(Expr, Expr)]): Map[Id, Expr] = {
    matching.foldLeft(Map.empty[Id, Expr]) { (acc, pair) =>
      (pair._1, pair._2) match {
        case (id: Id, expr: Expr) => acc + (id -> expr)
        case _                    => throw new Exception("The template expression can not be recursive")
      }
    }
  }

  def wrapRule(rule: Rule): RuleWrapper = {
    val allVariables        = rule.conditions.flatMap(_.variables).toSet ++ rule.conclusions.flatMap(_.variables).toSet
    val capturedVariables   = typing.HyperTypeChecker.getVariables(expr).toSet
    val freeVariables       = allVariables -- capturedVariables
    val freeVariableMapping = freeVariables.zipWithIndex.map { case (id, index) => id -> Id(s"<$index>") }.toMap
    val substitution        = Substitution(freeVariableMapping)
    val indexedRule         = substitution.apply(rule)
    if (freeVariables.isEmpty) {
      EmptyWrapper(indexedRule)
    } else {
      ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(typeSystem: TypeSystem, gamma: HyperMapping, delta: DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): ExpressionDerivationResult = {
    val context     = ExpressionDerivationContext(typeSystem, expr, gamma, delta, variableMapping, new Cache())
    val emptyResult = ExpressionDerivationResult(HyperTypeCollection(Set.empty), DeltaCollection(Map.empty))
    wrappedRules
      .foldLeft(emptyResult) { (acc, rule) =>
        rule.apply(context, acc, true).getExpressionResult
      }
      .getExpressionResult

  }
}

case class StatementDerivationRule(statement: StmtPattern, rules: Seq[Rule]) extends DerivationRule {

  val wrappedRules: Seq[RuleWrapper] = rules.map(rule => wrapRule(rule))

  def wrapRule(rule: Rule): RuleWrapper = {
    val allVariables        = rule.conditions.flatMap(_.variables).toSet ++ rule.conclusions.flatMap(_.variables).toSet
    val capturedVariables   = HyperTypeChecker.getVariables(statement).toSet
    val freeVariables       = allVariables -- capturedVariables
    val freeVariableMapping = freeVariables.zipWithIndex.map { case (id, index) => id -> Id(s"<$index>") }.toMap
    val substitution        = Substitution(freeVariableMapping)
    val indexedRule         = substitution.apply(rule)
    if (freeVariables.isEmpty) {
      EmptyWrapper(indexedRule)
    } else {
      ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(context: StatementDerivationContext): StatementDerivationResult = {
    val emptyResult   = StatementDerivationResult(HyperMapping(Map.empty), DeltaMapping(Map.empty))
    val derivedResult = wrappedRules.foldLeft(emptyResult) { (acc, rule) =>
      rule.apply(context, acc, false).getStatementResult
    }
    derivedResult
  }
}

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion], name: RuleName = RuleName.empty) extends CollectVariables {
  override def variables: ScalaSet[Id] = conditions.flatMap(_.variables).toSet ++ conclusions.flatMap(_.variables).toSet
}

trait StmtPattern {}

case class CompStmt(first: Id, second: Id)                       extends StmtPattern
case class AssignStmt(variable: Id, value: Id)                   extends StmtPattern
case class IfStmt(condition: Id, thenBranch: Id, elseBranch: Id) extends StmtPattern
case class InitStmt()                                            extends StmtPattern
case class HavocStmt(variable: Id)                               extends StmtPattern
case class MethodInitStmt(variable: Id)                          extends StmtPattern
