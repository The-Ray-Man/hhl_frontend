package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing.IntType
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.typing.dsl


case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection)               {}
case class ExpressionDerivationContext(val expression: Expr, val mapping: HyperMapping, val premisses: Seq[ExpressionDerivationResult]) {}


case class RuleCheckContext(variables: Seq[Id]) {}

abstract class RuleWrapper(rule: dsl.Rule) {
  def getActions(context: ExpressionDerivationContext): Seq[Action]
  def checkRules(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action]
}

case class ForanyVariableWrapper(numVars: Int, rule: dsl.Rule) extends RuleWrapper(rule: dsl.Rule) {

  override def checkRules(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = ???

  def getActions(context: ExpressionDerivationContext): Seq[Action] = {
    val variablesInExpression   = getVariables(context.expression)
    val variablesInHyperMapping = context.mapping.mapping.keySet.map(Id(_))
    val allVariables            = variablesInExpression ++ variablesInHyperMapping
    allVariables
      .map(id => {
        val ruleCheckContext = RuleCheckContext(Seq(id))
        checkRules(context, ruleCheckContext)
      })
      .toSeq
      .flatten
  }
}
case class EmptyWrapper(rule: dsl.Rule) extends RuleWrapper(rule: dsl.Rule) {

  override def checkRules(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = ???

  def getActions(context: ExpressionDerivationContext): Seq[Action] = {
    val ruleCheckContext = RuleCheckContext(Seq())
    checkRules(context, ruleCheckContext)
  }
}