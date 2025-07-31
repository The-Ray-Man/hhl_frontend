package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.ElementOf
import viper.HHLVerifier.typing.{Low, Pos, Zero, Neg, GreaterOne, LessOne, MonoUp, MonoDown, One}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.typing.rules.ExpressionOperator
import viper.HHLVerifier.typing.rules.DeltaContains
import viper.HHLVerifier.typing.rules.BinaryExpressionDerivationRule


case class AdditionCombineFunctionHypertype() extends binaryCombineFunction {
    val rules = Seq(
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(Low()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(MonoUp(Set(Id("monoId"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId"))))), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(MonoDown(Set(Id("monoId"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId"))))), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(One())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(One()))), 
        binaryFunctionImplication(Seq(ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(LessOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(One())), Seq(), Seq(ElementOf(One()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(LessOne())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(LessOne()))),
    )
}


case class AdditionCombineFunctionDeltatype() extends binaryCombineFunction {
    val rules = Seq(
        binaryFunctionImplication(Seq(), Seq(DeltaContains(Id("x"), Low())), Seq(ElementOf(Low())), Seq(), Seq(DeltaContains(Id("x"), Low()))),
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(), Seq(DeltaContains(Id("x"), Low())), Seq(DeltaContains(Id("x"), Low()))),
        binaryFunctionImplication(Seq(), Seq(DeltaContains(Id("x"), Low())), Seq(), Seq(DeltaContains(Id("x"), Low())), Seq(DeltaContains(Id("x"), Low()))),
    )
}


case class AdditionDerivationRule() extends BinaryExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: binaryCombineFunction = AdditionCombineFunctionHypertype()

  override val combineFunctionDelta: binaryCombineFunction = AdditionCombineFunctionDeltatype()

}