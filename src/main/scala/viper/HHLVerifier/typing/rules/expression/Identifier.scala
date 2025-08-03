package viper.HHLVerifier.typing.rules.expression


import viper.HHLVerifier.typing.rules.ExpressionOperator
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.NullaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.LookupAndAddHyperType
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.typing.rules.ForanyVariableWrapper
import viper.HHLVerifier.typing.rules.VarInHyperMapping
import viper.HHLVerifier.typing.rules.ExpressionIsVar
import viper.HHLVerifier.typing.rules.VarHasDeltaType
import viper.HHLVerifier.typing.Zero



case class IdentifierCombineFunctionHypertype() extends nullaryCombineFunction[HyperTypeConclusion] {
    val rules = Seq(
        ForanyVariableWrapper(nullaryFunctionImplication(Seq(VarInHyperMapping(0)), Seq(LookupAndAddHyperType(0)))),
    )
}


case class IdentifierCombineFunctionDeltatype() extends nullaryCombineFunction[DeltaConclusion] {
    val rules = Seq(
        ForanyVariableWrapper(nullaryFunctionImplication(Seq(ExpressionIsVar(0)), Seq(VarHasDeltaType(0, Zero())))),
    )
}


case class IdentifierDerivationRule() extends NullaryExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = IdentifierCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = IdentifierCombineFunctionDeltatype()

}