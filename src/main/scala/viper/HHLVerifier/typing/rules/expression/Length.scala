package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.unaryCombineFunction
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
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.typing.rules.RuleWrapper

case class LengthCombineFunctionHypertype(override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq()) extends unaryCombineFunction[HyperTypeConclusion](rules) {}

case class LengthCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends unaryCombineFunction[DeltaConclusion](rules) {}

case class LengthDerivationRule() extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case _ @LengthExpr(e) => super.applyUnary(system, expression, e, mapping)
      case _                => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }

  override val combineFunctionHypertype: unaryCombineFunction[HyperTypeConclusion] = LengthCombineFunctionHypertype()

  override val combineFunctionDelta: unaryCombineFunction[DeltaConclusion] = LengthCombineFunctionDeltatype()

}
