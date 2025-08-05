package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.{ExpressionDerivationRule => RuleExpressionDerivationRule}


object SpecificationUtil {

  def findExpressionRuleByName[C <: RuleExpressionDerivationRule](rules: Seq[(String, Any)], name: String): C = {
    rules.find(_._1 == name) match {
      case Some((_, rule)) => rule.asInstanceOf[C]
      case None            => throw new Exception(s"Rule |$name| not found in derivation rules")
    }
  }

  def combineSpecifications(specifications: Seq[Specification]): Specification = {
    
    val specificationCombined = specifications.reduce((acc, spec) => {
      SpecificationUtil.mergeSpecification(acc, spec)
    })
    specificationCombined
  }

  def mergeSpecification(spec1: Specification, spec2: Specification): Specification = {
    val expressionDerivationByOp = (spec1.derivationRules ++ spec2.derivationRules).filter(_.isInstanceOf[ExpressionDerivationRule]).map(_.asInstanceOf[ExpressionDerivationRule]).groupBy(_.op).map(_._2).toSeq
    val combinedRules = expressionDerivationByOp.map(rules => rules.reduce((rule1, rule2) => combineExpressionDerivationRules(rule1, rule2)))
    Specification(combinedRules)
  }

  def combineExpressionDerivationRules(rule1 : ExpressionDerivationRule, rule2: ExpressionDerivationRule): ExpressionDerivationRule = {
    assert(rule1.op == rule2.op, "Cannot combine rules with different operators")
    assert(rule1.inputs == rule2.inputs, "Cannot combine rules with different inputs")
    val rules = rule1.rules ++ rule2.rules
    ExpressionDerivationRule(rule1.op, rule1.inputs, rules)
  }
}
