package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.dsl.ast.{Rule}
import viper.HHLVerifier.typing.dsl.utils.Substitution

case class RuleCheckContext(variables: Map[Id, Id]) {}

abstract class RuleWrapper(rule: Rule) {
  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult

  def checkAndApply(context: Context, ruleCheckContext: RuleCheckContext, result: DerivationResult, expression: Boolean): DerivationResult = {
    val appliedIndexedRule = Substitution.apply(ruleCheckContext.variables, rule)
    //  println("\n\n", rule)
    val conditionHolds = appliedIndexedRule.conditions.forall(condition => { condition.check(context, expression) })

    if (conditionHolds) {
      val res = appliedIndexedRule.conclusions.foldLeft(result) { case (acc, conclusion) => conclusion.apply(context, acc) }
      res match {
        case exprRes: ExpressionDerivationResult => ExpressionDerivationResult(exprRes.hyperTypeCollection, exprRes.deltaCollection, exprRes.appliedRules.appended(rule.name))
        case stmtRes: StatementDerivationResult  => StatementDerivationResult(stmtRes.hyperTypeMapping, stmtRes.deltaMapping, stmtRes.appliedRules.appended(rule.name))
      }
    } else {
      result
    }
  }
}

case class ForanyVariableWrapper(numVars: Int, rule: Rule) extends RuleWrapper(rule: Rule) {

  def orderedSubsets[A](set: Seq[A], n: Int): Seq[Seq[A]] = {
    set.permutations
      .flatMap(_.sliding(n, 1)) // take consecutive chunks of length n
      .filter(_.length == n)
      .toSeq
      .distinct // remove duplicates if input has duplicates
  }

  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult = {
    val allRuleContext = orderedSubsets(context.allVars.toSeq, numVars).map { subset =>
      val mapping = (subset.zipWithIndex.map { case (id, index) => Id(s"<$index>") -> id }.toMap)
      RuleCheckContext(mapping)
    }

    val newResult = allRuleContext.foldLeft(result) { case (agg, capturedRuleContext) =>
      checkAndApply(context, capturedRuleContext, agg, expression)
    }
    newResult
  }
}
case class EmptyWrapper(rule: Rule) extends RuleWrapper(rule: Rule) {

  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult = {
    val ruleCheckContext = RuleCheckContext(Map())
    checkAndApply(context, ruleCheckContext, result, expression)
  }
}
