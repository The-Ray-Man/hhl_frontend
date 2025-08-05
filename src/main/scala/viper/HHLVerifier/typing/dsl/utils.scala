package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.ExpressionDerivationRule

object SpecificationUtil {

  def findExpressionRuleByName[C <: ExpressionDerivationRule](rules: Seq[(String, Any)], name: String): C = {
    rules.find(_._1 == name) match {
      case Some((_, rule)) => rule.asInstanceOf[C]
      case None            => throw new Exception(s"Rule |$name| not found in derivation rules")
    }
  }
}
