package viper.HHLVerifier.typing.rules.expression.unaryOp

import viper.HHLVerifier.typing.rules.binaryFunctionImplication
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.ElementOf
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
import viper.HHLVerifier.typing.rules.unaryCombineFunction
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.typing.rules.RuleWrapper

case class NotCombineFunctionHypertype(override val rules: Seq[RuleWrapper[HyperTypeConclusion]] = Seq()) extends unaryCombineFunction[HyperTypeConclusion](rules) {}
case class NotCombineFunctionDeltatype(
    override val rules: Seq[RuleWrapper[DeltaConclusion]] = Seq()
) extends unaryCombineFunction[DeltaConclusion](rules) {}

case class NotDerivationRule(val combineFunctionHypertype: unaryCombineFunction[HyperTypeConclusion] = NotCombineFunctionHypertype(), val combineFunctionDelta: unaryCombineFunction[DeltaConclusion] = NotCombineFunctionDeltatype()) extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case unaryExpr @ UnaryExpr(op, e) => {
        op match {
          case "!" => super.applyUnary(system, expression, e, mapping)
          case _   => throw new IllegalArgumentException(s"Wrong rule applied for unary operator: ${op}")
        }
      }
      case _ => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }
}
