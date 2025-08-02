package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.ExpressionOperator
import viper.HHLVerifier.typing.rules.ContainsHyperType
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.NullaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.ConstTrue
import viper.HHLVerifier.typing.rules.ConstFalse
import viper.HHLVerifier.typing.True
import viper.HHLVerifier.typing.False
import viper.HHLVerifier.typing.rules.EmptyWrapper



case class BooleanCombineFunctionHypertype() extends nullaryCombineFunction[HyperTypeConclusion] {
    val rules = Seq(
        EmptyWrapper(nullaryFunctionImplication(Seq(ConstTrue()), Seq(ContainsHyperType(True())))),
        EmptyWrapper(nullaryFunctionImplication(Seq(ConstFalse()), Seq(ContainsHyperType(False())))),
    )
}


case class BooleanCombineFunctionDeltatype() extends nullaryCombineFunction[DeltaConclusion] {
    val rules = Seq()
}


case class BooleanDerivationRule() extends NullaryExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = BooleanCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = BooleanCombineFunctionDeltatype()

}