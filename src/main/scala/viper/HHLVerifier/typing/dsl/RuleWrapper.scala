package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing.IntType
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.typing.dsl
import java.beans.Expression
import viper.HHLVerifier.typing.dsl.TypeSystem

case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection)                                                 {}
case class ExpressionDerivationContext(val typeSystem: TypeSystem, val expression: Expr, val gamma: HyperMapping, val delta: DeltaMapping, val varMapping: Map[Id, Expr]) {}

case class RuleCheckContext(variables: Map[Id, Id]) {}

abstract class RuleWrapper(rule: dsl.Rule) {
  def apply(context: ExpressionDerivationContext, result: ExpressionDerivationResult): ExpressionDerivationResult
  def checkAndApply(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext, result: ExpressionDerivationResult): ExpressionDerivationResult = {
    val conditionHolds = rule.conditions.forall(condition => condition.check(context, ruleCheckContext))

    if (conditionHolds) {
      rule.conclusions.foldLeft(result) { case (acc, conclusion) => conclusion.apply(context, ruleCheckContext, acc) }
    } else {
      result
    }
  }
}

case class ForanyVariableWrapper(numVars: Int, rule: dsl.Rule) extends RuleWrapper(rule: dsl.Rule) {

  def orderedSubsets[A](set: Seq[A], n: Int): Seq[Seq[A]] = {
    set.permutations
      .flatMap(_.sliding(n, 1)) // take consecutive chunks of length n
      .filter(_.length == n)
      .toSeq
      .distinct // remove duplicates if input has duplicates
  }

  def apply(context: ExpressionDerivationContext, result: ExpressionDerivationResult): ExpressionDerivationResult = {
    val variablesInExpression   = getVariables(context.expression)
    val variablesInHyperMapping = context.gamma.mapping.keySet.map(Id(_))
    val variablesInDeltaMapping = context.delta.collection.keySet.map(Id(_))
    val allVariables            = variablesInExpression ++ variablesInHyperMapping ++ variablesInDeltaMapping
    val allRuleContext          = orderedSubsets(allVariables.toSeq, numVars).map { subset =>
      val mapping = (subset.zipWithIndex.map { case (id, index) => Id(s"<$index>") -> id }.toMap)
      RuleCheckContext(mapping)
    }

    val newResult = allRuleContext.foldLeft(result) { case (agg, capturedRuleContext) =>
      checkAndApply(context, capturedRuleContext, agg)
    }
    newResult
  }
}
case class EmptyWrapper(rule: dsl.Rule) extends RuleWrapper(rule: dsl.Rule) {

  def apply(context: ExpressionDerivationContext, result: ExpressionDerivationResult): ExpressionDerivationResult = {
    val ruleCheckContext = RuleCheckContext(Map())
    checkAndApply(context, ruleCheckContext, result)
  }
}
