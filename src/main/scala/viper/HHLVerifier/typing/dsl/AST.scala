package viper.HHLVerifier.typing.dsl

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

case class Specification(hypertypeDeclaration: Seq[HyperTypeDeclaration], derivationRules: Seq[DerivationRule]) {

  def toTypeSystem(): TypeSystem = {
    val expressionRules = derivationRules.filter(_.isInstanceOf[ExpressionDerivationRule]).map(rule => renameVariables(rule.asInstanceOf[ExpressionDerivationRule]))
    val statementRules  = derivationRules.filter(_.isInstanceOf[StatementDerivationRule]).map(rule => renameVariables(rule.asInstanceOf[StatementDerivationRule]))
    val typeSystem      = TypeSystem(
      statementTypeSystem = StatementTypeSystem(
        assignRule = statementRules.find(rule => rule.statement.isInstanceOf[AssignStmt]).getOrElse(throw new Exception("No assign rule found")),
        branchRule = statementRules.find(rule => rule.statement.isInstanceOf[IfStmt]).getOrElse(throw new Exception("No branch rule found")),
        compositionRule = statementRules.find(rule => rule.statement.isInstanceOf[CompStmt]).getOrElse(throw new Exception("No composition rule found")),
        initRule = statementRules.find(rule => rule.statement.isInstanceOf[InitStmt]).getOrElse(throw new Exception("No init rule found")),
        havocRule = statementRules.find(rule => rule.statement.isInstanceOf[HavocStmt]).getOrElse(throw new Exception("No havoc rule found")),
        methodInitRule = statementRules.find(rule => rule.statement.isInstanceOf[MethodInitStmt]).getOrElse(throw new Exception("No method init rule found"))
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

  def renameVariables(rule: ExpressionDerivationRule): ExpressionDerivationRule = {
    val allVariables     = typing.HyperTypeChecker.getVariables(rule.expr) ++ rule.rules.flatMap(_.variables).toSet
    val renamedVariables = allVariables.map { case id => id -> Id(s"VAR'${id.name}'") }.toMap
    val renamedRules     = rule.rules.map(rule => applyIndexed.applyIndexed(renamedVariables, rule))
    val renamedExpr      = applyIndexed.applyIndexed(renamedVariables, rule.expr)
    ExpressionDerivationRule(renamedExpr, renamedRules)
  }

  def renameVariables(rule: StatementDerivationRule): StatementDerivationRule = {
    val allVariables     = typing.HyperTypeChecker.getVariables(rule.statement) ++ rule.rules.flatMap(_.variables).toSet
    val renamedVariables = allVariables.map { case id => id -> Id(s"VAR'${id.name}'") }.toMap
    val renamedRules     = rule.rules.map(rule => applyIndexed.applyIndexed(renamedVariables, rule))
    val renamedStmt      = applyIndexed.applyIndexed(renamedVariables, rule.statement)
    StatementDerivationRule(renamedStmt, renamedRules)
  }

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
    val freeVariableMapping = freeVariables.zipWithIndex.toMap
    val indexedRule         = ToIndexed.toIndexedVariable(freeVariableMapping, rule)
    if (freeVariables.isEmpty) {
      EmptyWrapper(indexedRule)
    } else {
      ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): ExpressionDerivationResult = {
    val context     = ExpressionDerivationContext(typeSystem, expr, typeSystem.allVariables, gamma, delta, variableMapping, new Cache())
    val emptyResult = ExpressionDerivationResult(typing.HyperTypeCollection(Set.empty), typing.DeltaCollection(Map.empty))
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
    val capturedVariables   = typing.HyperTypeChecker.getVariables(statement).toSet
    val freeVariables       = allVariables -- capturedVariables
    val freeVariableMapping = freeVariables.zipWithIndex.toMap
    val indexedRule         = ToIndexed.toIndexedVariable(freeVariableMapping, rule)
    if (freeVariables.isEmpty) {
      EmptyWrapper(indexedRule)
    } else {
      ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(context: StatementDerivationContext): StatementDerivationResult = {
    val emptyResult   = StatementDerivationResult(typing.HyperMapping(Map.empty), typing.DeltaMapping(Map.empty))
    val derivedResult = wrappedRules.foldLeft(emptyResult) { (acc, rule) =>
      rule.apply(context, acc, false).getStatementResult
    }
    derivedResult
  }
}

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion], name: RuleName = RuleName.empty) extends CollectVariables {
  override def variables: ScalaSet[Id] = conditions.flatMap(_.variables).toSet ++ conclusions.flatMap(_.variables).toSet
}

trait ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
}

trait CollectVariables {
  def variables: scala.collection.immutable.Set[Id]
}

// Building Blocks for Condition and Conclusion
trait Derivation
trait Mapping extends ConclusionInfo with CollectVariables
trait Set     extends ConclusionInfo with CollectVariables

case class WithoutElement(set: Set, elem: Element) extends Set {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = set.variables ++ elem.variables
}

case class AssignedVariables(stmt: Id) extends Set {
  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(stmt)
}

case class ProgramContext() extends Set {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

}

case class Variables(content: Id) extends Set {
  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(content)
}

case class AllVariables() extends Set {
  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = false
}

case class AllParameters() extends Set {
  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]
}

case class HyperTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Set with Derivation {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(expr) ++ gamma.variables ++ delta.variables

}

case class DeriveHyperType(expr: Id, gamma: Mapping, delta: Mapping, context: Set) extends Mapping with Derivation {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(expr) ++ gamma.variables ++ delta.variables ++ context.variables

}

case class DeriveDeltaType(expr: Id, gamma: Mapping, delta: Mapping, context: Set) extends Mapping with Derivation {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(expr) ++ context.variables ++ gamma.variables ++ delta.variables

}

case class InitializeDeltaMapping() extends Mapping with Derivation {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set.empty
}

case class InitializeGammaMapping() extends Mapping with Derivation {

  override def isHyperTypeConclusion(): Boolean = true

  override def variables: immutable.Set[Id] = immutable.Set.empty
}

case class DeltaTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Mapping with Derivation {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(expr) ++ gamma.variables ++ delta.variables
}

case class HyperCollectionResult() extends Set {

  override def isHyperTypeConclusion(): Boolean = true
  override def variables: immutable.Set[Id]     = immutable.Set.empty[Id]
}
case class MappingAccess(mapping: Mapping, id: Id) extends Set with Mapping {

  override def isHyperTypeConclusion(): Boolean = mapping.isHyperTypeConclusion()
  override def variables: immutable.Set[Id]     = scala.collection.immutable.Set(id) ++ mapping.variables
}

case class DeltaCollectionResult() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = false

}
case class Gamma() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = true

}

case class Delta() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = false

}

case class GammaResult() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = true

}

case class DeltaResult() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

  override def isHyperTypeConclusion(): Boolean = false

}

trait StmtPattern {}

case class CompStmt(first: Id, second: Id)                       extends StmtPattern
case class AssignStmt(variable: Id, value: Id)                   extends StmtPattern
case class IfStmt(condition: Id, thenBranch: Id, elseBranch: Id) extends StmtPattern
case class InitStmt()                                extends StmtPattern
case class HavocStmt(variable: Id)                               extends StmtPattern
case class MethodInitStmt(variable: Id)                          extends StmtPattern
