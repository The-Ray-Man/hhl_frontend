package viper.HHLVerifier.typing.rules.expression.binaryOp

import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.typing.rules.RuleWrapper
import viper.HHLVerifier.ast.ImpliesExpr

case class ImplicationCombineFunctionHypertype(override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq()) extends binaryCombineFunction[HyperTypeConclusion](rules) {}

case class ImplicationCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends binaryCombineFunction[DeltaConclusion](rules) {}

case class ImplicationDerivationRule(val combineFunctionHypertype: binaryCombineFunction[HyperTypeConclusion] = ImplicationCombineFunctionHypertype(), val combineFunctionDelta: binaryCombineFunction[DeltaConclusion] = ImplicationCombineFunctionDeltatype()) extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram] // Placeholder for soundness tests, if needed

  override def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case ImpliesExpr(e1, e2) => super.applyBinary(system, expression, e1, e2, mapping)
      case _                                   => throw new IllegalArgumentException(s"Wrong rule applied!")
    }
  }
}
