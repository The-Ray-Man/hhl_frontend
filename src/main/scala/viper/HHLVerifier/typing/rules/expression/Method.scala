package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
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
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.typing.rules.RuleWrapper

case class MethodCombineFunctionHypertype(override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq()) extends nullaryCombineFunction[HyperTypeConclusion](rules) {}

case class MethodCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends nullaryCombineFunction[DeltaConclusion](rules) {}

case class MethodDerivationRule(val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = MethodCombineFunctionHypertype(), val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = MethodCombineFunctionDeltatype()) extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case _ => throw new IllegalArgumentException(s"Function Expressions are not implemented yet")
    }
  }

}
