package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.dsl
import java.beans.Expression
import viper.HHLVerifier.typing.dsl.TypeSystem
import viper.HHLVerifier.ast.Stmt
import scala.collection.immutable

case class RuleCheckContext(variables: Map[Id, Id]) {}

abstract class RuleWrapper(rule: dsl.Rule) {
  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult

  def checkAndApply(context: Context, ruleCheckContext: RuleCheckContext, result: DerivationResult, expression: Boolean): DerivationResult = {

    val appliedIndexedRule = applyIndexed.applyIndexed(ruleCheckContext.variables, rule)
    val conditionHolds     = appliedIndexedRule.conditions.forall(condition => {println(condition);condition.check(context, expression)})

    if (conditionHolds) {
      appliedIndexedRule.conclusions.foldLeft(result) { case (acc, conclusion) => println(conclusion); conclusion.apply(context, acc) }
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
case class EmptyWrapper(rule: dsl.Rule) extends RuleWrapper(rule: dsl.Rule) {

  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult = {
    val ruleCheckContext = RuleCheckContext(Map())
    checkAndApply(context, ruleCheckContext, result, expression)
  }
}
