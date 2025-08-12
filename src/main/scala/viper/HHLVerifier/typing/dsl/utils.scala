package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.{ExpressionDerivationRule => RuleExpressionDerivationRule}
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.silicon.state.terms.BinaryOp
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.silicon.state.terms.UnaryOp


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
    throw new Exception("Not yet implemented: 1. Match the expression, 2. replace the expression in all the rules. 3. Combine rules")
    // val expressionDerivationByOp = (spec1.derivationRules ++ spec2.derivationRules).filter(_.isInstanceOf[ExpressionDerivationRule]).map(_.asInstanceOf[ExpressionDerivationRule]).groupBy(_.op).map(_._2).toSeq
    // val combinedRules = expressionDerivationByOp.map(rules => rules.reduce((rule1, rule2) => combineExpressionDerivationRules(rule1, rule2)))

    // val combinedHypertypeDeclarations = spec1.hypertypeDeclaration ++ spec2.hypertypeDeclaration
    // Specification(combinedHypertypeDeclarations, combinedRules)
  }

  def combineExpressionDerivationRules(rule1 : ExpressionDerivationRule, rule2: ExpressionDerivationRule): ExpressionDerivationRule = {
    throw new Exception("Not yet implemented: 1. Match the expression, 2. replace the expression in all the rules. 3. Combine rules")
    // assert(samePlaceholderExpression(rule1.expr, rule2.expr), "Cannot combine rules with different expressions")
    // val rules = rule1.rules ++ rule2.rules
    // ExpressionDerivationRule(rule1.expr, rules)
  }

}
