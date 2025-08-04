package viper.HHLVerifier.typing.rules.expression

import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.DeltaConclusion
import viper.HHLVerifier.typing.rules.nullaryCombineFunction
import viper.HHLVerifier.typing.rules.nullaryFunctionImplication
import viper.HHLVerifier.typing.rules.LookupAndAddHyperType
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.typing.rules.ForanyVariableWrapper
import viper.HHLVerifier.typing.rules.VarInHyperMapping
import viper.HHLVerifier.typing.rules.ExpressionIsVar
import viper.HHLVerifier.typing.rules.VarHasDeltaType
import viper.HHLVerifier.typing.Zero
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.{DeltaCollection, HyperMapping, HyperTypeCollection}
import viper.HHLVerifier.typing.rules.ExpressionSystem
import viper.HHLVerifier.ast.Id

case class IdentifierCombineFunctionHypertype() extends nullaryCombineFunction[HyperTypeConclusion] {
  val rules = Seq(
    ForanyVariableWrapper(nullaryFunctionImplication(Seq(VarInHyperMapping(0)), Seq(LookupAndAddHyperType(0))))
  )
}

case class IdentifierCombineFunctionDeltatype() extends nullaryCombineFunction[DeltaConclusion] {
  val rules = Seq(
    ForanyVariableWrapper(nullaryFunctionImplication(Seq(ExpressionIsVar(0)), Seq(VarHasDeltaType(0, Zero()))))
  )
}

case class IdentifierDerivationRule() extends ExpressionDerivationRule {

  override def generateSoundnessTests: Seq[HHLProgram] = Seq.empty[HHLProgram]

  override def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
    expression match {
      case _ @Id(_) => applyNullary(system, expression, mapping)
      case _        => throw new IllegalArgumentException(s"Wrong rule applied for expression: ${expression}")
    }
  }

  override val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion] = IdentifierCombineFunctionHypertype()

  override val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion] = IdentifierCombineFunctionDeltatype()

}
