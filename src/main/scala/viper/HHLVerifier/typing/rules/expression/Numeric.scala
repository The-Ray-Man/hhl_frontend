package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.ElementOf
import viper.HHLVerifier.typing.{Low, Pos, Zero, Neg, GreaterOne, LessOne, MonoUp, MonoDown, One}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.rules.ExpressionOperator
import viper.HHLVerifier.typing.rules.DeltaContains
import viper.HHLVerifier.typing.rules.BinaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.ContainsHyperType
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.VarHasDeltaType
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.NullaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.{ConstAbsGtOne, ConstAbsLtOne, ConstAbsOne, ConstPositiveInt, ConstNegativeInt}
import viper.HHLVerifier.typing.rules.ConstZeroInt



case class NumericalCombineFunctionHypertype() extends nullaryCombineFunction[HyperTypeConclusion] {
    val rules = Seq(
        nullaryFunctionImplication(Seq(ConstAbsGtOne()), Seq(ContainsHyperType(GreaterOne()))),
        nullaryFunctionImplication(Seq(ConstAbsLtOne()), Seq(ContainsHyperType(LessOne()))),
        nullaryFunctionImplication(Seq(ConstAbsOne()), Seq(ContainsHyperType(One()))),
        nullaryFunctionImplication(Seq(ConstPositiveInt()), Seq(ContainsHyperType(Pos()))),
        nullaryFunctionImplication(Seq(ConstNegativeInt()), Seq(ContainsHyperType(Neg()))),
        nullaryFunctionImplication(Seq(ConstZeroInt()), Seq(ContainsHyperType(Zero()))),
    )
}


case class NumericalCombineFunctionDeltatype() extends nullaryCombineFunction[DeltaConclusion] {
    val rules = Seq()
}


case class NumericalDerivationRule() extends NullaryExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = NumericalCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = NumericalCombineFunctionDeltatype()

}