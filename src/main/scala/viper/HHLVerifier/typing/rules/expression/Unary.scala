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
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.typing.rules.expression.unaryOp.MinusDerivationRule
import viper.HHLVerifier.typing.rules.CombineFunction

case class UnaryExpressionDerivationRule(
    val minusRule: ExpressionDerivationRule = MinusDerivationRule()
    // TODO: Add other unary rules here

) extends ExpressionDerivationRule {

  override val combineFunctionHypertype: CombineFunction[HyperTypeConclusion] = null

  override val combineFunctionDelta: CombineFunction[DeltaConclusion] = null

  def generateSoundnessTests: Seq[HHLProgram]                                                                           = Seq()
  def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {

    expression match {
      case unaryExpr @ UnaryExpr(_, _) => {
        unaryExpr.op match {
          case "-" => minusRule.derive(system, expression, mapping)
          case _   => throw new Exception("Unsupported unary operator: " + unaryExpr.op)
        }
      }
      case _ => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }

  }
}
