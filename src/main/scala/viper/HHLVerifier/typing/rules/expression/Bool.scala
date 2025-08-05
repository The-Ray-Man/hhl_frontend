package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.ContainsHyperType
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.rules.ExpressionDerivationContext
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.rules.RuleWrapper

case class BooleanCombineFunctionHypertype(
    override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq(
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstTrue()), Seq(ContainsHyperType(True())))),
      // EmptyWrapper(nullaryFunctionImplication(Seq(ConstFalse()), Seq(ContainsHyperType(False()))))
    )
) extends nullaryCombineFunction[HyperTypeConclusion](rules) {}

case class BooleanCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends nullaryCombineFunction[DeltaConclusion](rules) {}

case class BooleanDerivationRule(val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = BooleanCombineFunctionHypertype(), val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = BooleanCombineFunctionDeltatype()) extends ExpressionDerivationRule {

  def generateSoundnessTests: Seq[viper.HHLVerifier.ast.HHLProgram]                                                         = Seq()
  def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case BoolLit(value) => applyNullary(system, expression, mapping)
      case _              => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }

}
