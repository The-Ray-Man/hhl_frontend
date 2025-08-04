package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing.rules.ExpressionSystem
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.typing.rules.CombineFunction

case class BinaryExpressionDerivationRule(
    val additionRule: ExpressionDerivationRule = binaryOp.AdditionDerivationRule()
    // TODO: Add other binary rules here
) extends ExpressionDerivationRule {

  override val combineFunctionHypertype: CombineFunction[HyperTypeConclusion] = null

  override val combineFunctionDelta: CombineFunction[DeltaConclusion] = null

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case binaryExpr @ BinaryExpr(e1, op, e2) => {
        op match {
          case "+" => additionRule.derive(system, expression, mapping)
          case _   => throw new Exception("Unsupported binary operator: " + op)
        }
      }
      case _ => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }
}
