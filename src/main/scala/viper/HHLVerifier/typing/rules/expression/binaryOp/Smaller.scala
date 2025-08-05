package viper.HHLVerifier.typing.rules.expression.binaryOp

import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.typing.rules.RuleWrapper

case class SmallerCombineFunctionHypertype(override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq()) extends binaryCombineFunction[HyperTypeConclusion](rules) {}

case class SmallerCombineFunctionDeltatype(override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()) extends binaryCombineFunction[DeltaConclusion](rules) {}

case class SmallerDerivationRule(val combineFunctionHypertype: binaryCombineFunction[HyperTypeConclusion] = SmallerCombineFunctionHypertype(), val combineFunctionDelta: binaryCombineFunction[DeltaConclusion] = SmallerCombineFunctionDeltatype()) extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram] // Placeholder for soundness tests, if needed

  override def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case BinaryExpr(e1, op, e2) => super.applyBinary(system, expression, e1, e2, mapping)
      case _                                   => throw new IllegalArgumentException(s"Wrong rule applied!")
    }
  }
}
