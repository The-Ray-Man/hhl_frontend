package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.typing.rules.binaryCombineFunction
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.typing.rules.CombineFunction
import viper.HHLVerifier.ast.ImpliesExpr

case class BinaryExpressionDerivationRule(
    val additionRule: ExpressionDerivationRule = binaryOp.AdditionDerivationRule(),
    val andRule: ExpressionDerivationRule = binaryOp.AndDerivationRule(),
    val divisionRule: ExpressionDerivationRule = binaryOp.DivisionDerivationRule(),
    val equalityRule: ExpressionDerivationRule = binaryOp.EqualityDerivationRule(),
    val extensionRule: ExpressionDerivationRule = binaryOp.ExtensionDerivationRule(),
    val greaterRule: ExpressionDerivationRule = binaryOp.GreaterDerivationRule(),
    val greaterEqualRule: ExpressionDerivationRule = binaryOp.GreaterEqualDerivationRule(),
    val implicationRule: ExpressionDerivationRule = binaryOp.ImplicationDerivationRule(),
    val inRule: ExpressionDerivationRule = binaryOp.InDerivationRule(),
    val inequalityRule: ExpressionDerivationRule = binaryOp.InequalityDerivationRule(),
    val moduloRule: ExpressionDerivationRule = binaryOp.ModuloDerivationRule(),
    val multiplicationRule: ExpressionDerivationRule = binaryOp.MultiplicationDerivationRule(),
    val orRule: ExpressionDerivationRule = binaryOp.OrDerivationRule(),
    val setminusRule: ExpressionDerivationRule = binaryOp.SetminusDerivationRule(),
    val smallerRule: ExpressionDerivationRule = binaryOp.SmallerDerivationRule(),
    val smallerEqualRule: ExpressionDerivationRule = binaryOp.SmallerEqualDerivationRule(),
    val subtractionRule: ExpressionDerivationRule = binaryOp.SubtractionDerivationRule(),
    val unionRule: ExpressionDerivationRule = binaryOp.UnionDerivationRule()
) extends ExpressionDerivationRule {

  override val combineFunctionHypertype: CombineFunction[HyperTypeConclusion] = null

  override val combineFunctionDelta: CombineFunction[DeltaConclusion] = null

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionTypeSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case binaryExpr @ BinaryExpr(e1, op, e2) => {
        op match {
          case "+"        => additionRule.derive(system, expression, mapping)
          case "&&"       => andRule.derive(system, expression, mapping)
          case "/"        => divisionRule.derive(system, expression, mapping)
          case "=="       => equalityRule.derive(system, expression, mapping)
          case "++"       => extensionRule.derive(system, expression, mapping)
          case ">"        => greaterRule.derive(system, expression, mapping)
          case ">="       => greaterEqualRule.derive(system, expression, mapping)
          case "in"       => inRule.derive(system, expression, mapping)
          case "!="       => inequalityRule.derive(system, expression, mapping)
          case "%"        => moduloRule.derive(system, expression, mapping)
          case "*"        => multiplicationRule.derive(system, expression, mapping)
          case "||"       => orRule.derive(system, expression, mapping)
          case "setminus" => setminusRule.derive(system, expression, mapping)
          case "<"        => smallerRule.derive(system, expression, mapping)
          case "<="       => smallerEqualRule.derive(system, expression, mapping)
          case "-"        => subtractionRule.derive(system, expression, mapping)
          case "union"    => unionRule.derive(system, expression, mapping)
          case _          => throw new Exception("Unsupported binary operator: " + op)
        }
      }
      case _ @ImpliesExpr(_, _) => implicationRule.derive(system, expression, mapping)
      case _                    => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }
}
