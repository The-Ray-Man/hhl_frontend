package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.dsl.ast.{Rule}
import viper.HHLVerifier.typing.dsl.utils.Substitution

/** Context for checking rules. The context contains a mapping from free variables to actual program variables.
  */
case class RuleCheckContext(variables: Map[Id, Id]) {}

/** Abstract wrapper for rules. These wrappers are used to assign program variables to free variables in the rule.
  *
  * @param rule
  */
abstract class RuleWrapper(rule: Rule) {
  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult

  def checkAndApply(context: Context, ruleCheckContext: RuleCheckContext, result: DerivationResult, expression: Boolean): DerivationResult = {
    val substitution       = Substitution(ruleCheckContext.variables)
    val appliedIndexedRule = substitution.apply(rule)
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

/** This wrapper assignes every possible combination of program variables to the free variables. it applies all the different rules one after each other.
  *
  * @param numVars
  *   number of unique free variables in the rule.
  */
case class ForanyVariableWrapper(numVars: Int, rule: Rule) extends RuleWrapper(rule: Rule) {

  def assignments[A](elems: Seq[A], n: Int): Seq[Seq[A]] = {
    if (n == 0) Seq(Seq())
    else {
      for {
        e    <- elems
        rest <- assignments(elems, n - 1)
      } yield e +: rest
    }
  }

  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult = {
    val allRuleContext = assignments(context.allVars.toSeq, numVars).map { subset =>
      val mapping = (subset.zipWithIndex.map { case (id, index) => Id(s"<$index>") -> id }.toMap)
      RuleCheckContext(mapping)
    }

    val newResult = allRuleContext.foldLeft(result) { case (agg, capturedRuleContext) =>
      checkAndApply(context, capturedRuleContext, agg, expression)
    }
    newResult
  }
}

/** This wrapper is used if there are no free variables in the rule. The wrapper does nothing.
  */
case class EmptyWrapper(rule: Rule) extends RuleWrapper(rule: Rule) {

  def apply(context: Context, result: DerivationResult, expression: Boolean): DerivationResult = {
    val ruleCheckContext = RuleCheckContext(Map())
    checkAndApply(context, ruleCheckContext, result, expression)
  }
}
