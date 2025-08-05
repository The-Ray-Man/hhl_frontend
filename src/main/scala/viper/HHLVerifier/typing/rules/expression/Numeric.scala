package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.ElementOf
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.rules.DeltaContains
import viper.HHLVerifier.typing.rules.ContainsHyperType
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.VarHasDeltaType
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.{ConstAbsGtOne, ConstAbsLtOne, ConstAbsOne, ConstPositiveInt, ConstNegativeInt}
import viper.HHLVerifier.typing.rules.ConstZeroInt
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionSystem
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.rules.RuleWrapper

case class NumericalCombineFunctionHypertype(
    override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq(
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstAbsGtOne()), Seq(ContainsHyperType(GreaterOne())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstAbsLtOne()), Seq(ContainsHyperType(LessOne())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstAbsOne()), Seq(ContainsHyperType(One())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstPositiveInt()), Seq(ContainsHyperType(Pos())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstNegativeInt()), Seq(ContainsHyperType(Neg())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstZeroInt()), Seq(ContainsHyperType(Zero())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(), Seq(ContainsHyperType(Low()))))
    )
) extends nullaryCombineFunction[HyperTypeConclusion](rules) {}

case class NumericalCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends nullaryCombineFunction[DeltaConclusion](rules) {}

case class NumericalDerivationRule() extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case _ @Num(_) => super.applyNullary(system, expression, mapping)
      case _         => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = NumericalCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = NumericalCombineFunctionDeltatype()

}
