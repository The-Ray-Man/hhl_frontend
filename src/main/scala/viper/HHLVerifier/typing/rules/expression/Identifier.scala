package viper.HHLVerifier.typing.rules.expression


import viper.HHLVerifier.typing.rules.ExpressionOperator
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.NullaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.LookupAndAddHyperType
import viper.HHLVerifier.typing.rules.EmptyWrapper



case class IdentifierCombineFunctionHypertype() extends nullaryCombineFunction[HyperTypeConclusion] {
    val rules = Seq(
        EmptyWrapper(nullaryFunctionImplication(Seq(), Seq(LookupAndAddHyperType()))),
    )
}


case class IdentifierCombineFunctionDeltatype() extends nullaryCombineFunction[DeltaConclusion] {
    val rules = Seq(
        EmptyWrapper(nullaryFunctionImplication(Seq(), Seq())),
    )
}


case class IdentifierDerivationRule() extends NullaryExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = IdentifierCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = IdentifierCombineFunctionDeltatype()

}