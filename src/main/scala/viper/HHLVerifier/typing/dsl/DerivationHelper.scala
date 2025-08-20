package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Expr, Stmt, Id}
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables

abstract class Context {
  var cache: Cache
  def typeSystem: TypeSystem
  def gamma: HyperMapping
  def delta: DeltaMapping
  def expr: Expr
  def varExprMapping: Map[Id, Expr]
  def getExprById(id: Id): Expr = varExprMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
  def varStmtMapping: Map[Id, Stmt]
  def getStmtById(id: Id): Stmt = varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in statement mapping"))
  def statement: Stmt
  def pc: HyperTypeCollection
  def variables: ScalaSet[Id]
  def addToExpressionCache(result: ExpressionDerivationResult): Unit = {
    cache.add(gamma, delta, expr, result)
  }
}

case class ExpressionDerivationContext(val typeSystem: TypeSystem, val expression: Expr, val gamma: HyperMapping, val delta: DeltaMapping, val varMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def varExprMapping: Map[Id, Expr] = varMapping

  override def varStmtMapping: Map[Id, Stmt] = throw new Exception("ExpressionDerivationContext does not have a statement mapping")

  override def expr: Expr = expression

  override def statement: Stmt = throw new Exception("ExpressionDerivationContext does not have a statement")

  override def pc: HyperTypeCollection = throw new Exception("ExpressionDerivationContext does not have a program counter (pc)")

  override def variables: ScalaSet[Id] = getVariables(expression)

}
case class StatementDerivationContext(val typeSystem: TypeSystem, val stmt: Stmt, val gamma: HyperMapping, val delta: DeltaMapping, val pc: HyperTypeCollection, val varStmtMapping: Map[Id, Stmt], val varExprMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def expr: Expr = throw new Exception("StatementDerivationContext does not have an expression")

  override def statement: Stmt = stmt

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id] // TODO

}

trait DerivationResult {
  def getExpressionResult: ExpressionDerivationResult
  def getStatementResult: StatementDerivationResult
}

case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection) extends DerivationResult {

  override def getExpressionResult: ExpressionDerivationResult = this

  override def getStatementResult: StatementDerivationResult = throw new Exception("ExpressionDerivationResult does not have a statement result")

}

case class StatementDerivationResult(val hyperTypeMapping: HyperMapping, val deltaMapping: DeltaMapping) extends DerivationResult {
  override def getExpressionResult: ExpressionDerivationResult = throw new Exception("StatementDerivationResult does not have an expression result")

  override def getStatementResult: StatementDerivationResult = this
}
