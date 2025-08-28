package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Expr, Stmt, Id}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.dsl
import viper.HHLVerifier.typing.dsl.utils.Cache

abstract class Context {
  var cache: Cache
  def typeSystem: TypeSystem
  def gamma: HyperMapping
  def delta: DeltaMapping
  def expr: Expr
  def allVars: ScalaSet[Id]
  def varExprMapping: Map[Id, Expr]
  def getExprById(id: Id): Expr = varExprMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
  def varStmtMapping: Map[Id, Stmt]
  def getStmtById(id: Id): Stmt = varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in statement mapping"))
  def statement: Stmt
  def variables: ScalaSet[Id]
  def addToExpressionCache(result: ExpressionDerivationResult): Unit = {
    cache.add(gamma, delta, expr, result)
  }
  def applyIndexed(elem: Element): Element = {
    val mapping = varExprMapping.filter { case (k, v) => v.isInstanceOf[Id] }.map { case (k, v) => (k, v.asInstanceOf[Id]) }
    utils.applyIndexed.applyIndexed(mapping, elem)
  }
}

case class ExpressionDerivationContext(val typeSystem: TypeSystem, val expression: Expr, val allVars: ScalaSet[Id], val gamma: HyperMapping, val delta: DeltaMapping, val varMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def varExprMapping: Map[Id, Expr] = varMapping

  override def varStmtMapping: Map[Id, Stmt] = throw new Exception("ExpressionDerivationContext does not have a statement mapping")

  override def expr: Expr = expression

  override def statement: Stmt = throw new Exception("ExpressionDerivationContext does not have a statement")

  override def variables: ScalaSet[Id] = getVariables(expression)

}
case class StatementDerivationContext(val typeSystem: TypeSystem, val stmt: Stmt, val allVars: ScalaSet[Id], val gamma: HyperMapping, val delta: DeltaMapping, val varStmtMapping: Map[Id, Stmt], val varExprMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def expr: Expr = throw new Exception("StatementDerivationContext does not have an expression")

  override def statement: Stmt = stmt

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id] // TODO

}

trait DerivationResult {
  def getExpressionResult: ExpressionDerivationResult
  def getStatementResult: StatementDerivationResult
  def appliedRules: Seq[RuleName]
}

case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection, val appliedRules: Seq[RuleName] = Seq.empty) extends DerivationResult {

  override def getExpressionResult: ExpressionDerivationResult = this

  override def getStatementResult: StatementDerivationResult = throw new Exception("ExpressionDerivationResult does not have a statement result")

}

case class StatementDerivationResult(val hyperTypeMapping: HyperMapping, val deltaMapping: DeltaMapping, val appliedRules: Seq[RuleName] = Seq.empty) extends DerivationResult {
  override def getExpressionResult: ExpressionDerivationResult = throw new Exception("StatementDerivationResult does not have an expression result")

  override def getStatementResult: StatementDerivationResult = this

}
