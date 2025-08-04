package viper.HHLVerifier.typing.rules.expression.binaryOp

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.ElementOf
import viper.HHLVerifier.typing.{Low, Pos, Zero, Neg, GreaterOne, LessOne, MonoUp, MonoDown, One}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.rules.DeltaContains
import viper.HHLVerifier.typing.rules.ContainsHyperType
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.VarHasDeltaType
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.typing.rules.ForanyVariableWrapper
import viper.HHLVerifier.typing.rules.VarNotInDelta
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionSystem
import viper.HHLVerifier.ast.BinaryExpr

case class AdditionCombineFunctionHypertype() extends binaryCombineFunction[HyperTypeConclusion] {
  val rules = Seq(
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(Low())), Seq(), Seq(), Seq(ContainsHyperType(Low())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(Pos())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(Pos())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(Pos())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(Neg())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(Neg())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(Neg())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(), Seq(ContainsHyperType(Pos())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(), Seq(ContainsHyperType(Pos())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(), Seq(ContainsHyperType(Neg())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(), Seq(ContainsHyperType(Neg())))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(MonoUp(Set(Id("0"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(), Seq(ContainsHyperType(MonoUp(Set(Id("0"))))))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoUp(Set(Id("0"))))), Seq(), Seq(), Seq(ContainsHyperType(MonoUp(Set(Id("0"))))))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(MonoDown(Set(Id("0"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(), Seq(ContainsHyperType(MonoDown(Set(Id("0"))))))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoDown(Set(Id("0"))))), Seq(), Seq(), Seq(ContainsHyperType(MonoDown(Set(Id("0"))))))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(One())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(One())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(LessOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(LessOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(One())), Seq(), Seq(), Seq(ContainsHyperType(One())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(LessOne())), Seq(), Seq(), Seq(ContainsHyperType(LessOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(GreaterOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Neg())), Seq(), Seq(), Seq(ContainsHyperType(LessOne())))),
    EmptyWrapper(binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(), Seq(ContainsHyperType(LessOne()))))
  )
}

case class AdditionCombineFunctionDeltatype() extends binaryCombineFunction[DeltaConclusion] {
  val rules = Seq(
    ForanyVariableWrapper(binaryFunctionImplication(Seq(), Seq(DeltaContains(0, Low())), Seq(ElementOf(Low())), Seq(), Seq(), Seq(VarHasDeltaType(0, Low())))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(), Seq(DeltaContains(0, Low())), Seq(), Seq(VarHasDeltaType(0, Low())))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(), Seq(DeltaContains(0, Low())), Seq(), Seq(DeltaContains(0, Low())), Seq(), Seq(VarHasDeltaType(0, Low())))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(VarNotInDelta(0)), Seq(), Seq(DeltaContains(0, Zero())), Seq(), Seq(VarHasDeltaType(0, Pos())))),
    ForanyVariableWrapper(binaryFunctionImplication(Seq(), Seq(DeltaContains(0, Zero())), Seq(ElementOf(Pos())), Seq(VarNotInDelta(0)), Seq(), Seq(VarHasDeltaType(0, Pos()))))
  )
}

case class AdditionDerivationRule() extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram] // Placeholder for soundness tests, if needed

  override def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case binaryExpr @ BinaryExpr(e1, op, e2) => super.applyBinary(system, expression, e1, e2, mapping)
      case _                                   => throw new IllegalArgumentException(s"Wrong rule applied!")
    }
  }

  override val combineFunctionHypertype: binaryCombineFunction[HyperTypeConclusion] = AdditionCombineFunctionHypertype()

  override val combineFunctionDelta: binaryCombineFunction[DeltaConclusion] = AdditionCombineFunctionDeltatype()

}
