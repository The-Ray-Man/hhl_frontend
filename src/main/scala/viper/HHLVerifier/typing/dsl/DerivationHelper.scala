package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Expr, Stmt, Id}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.dsl
import viper.HHLVerifier.typing.dsl.utils.Cache

/** Abstract class for representing a context in the type system.
  */
abstract class Context {
  var cache: Cache
  def typeSystem: TypeSystem
  def gamma: HyperMapping
  def delta: DeltaMapping
  def expr: Expr
  def allVars = typeSystem.allVariables
  def varExprMapping: Map[Id, Expr]
  def getExprById(id: Id): Expr = varExprMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
  def varStmtMapping: Map[Id, Stmt]
  def getStmtById(id: Id): Stmt = varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in statement mapping"))
  def statement: Stmt
  def variables: ScalaSet[Id]
  def addToExpressionCache(result: ExpressionDerivationResult): Unit = {
    cache.add(gamma, delta, expr, result)
  }
  def apply(elem: Element): Element = {
    val mapping      = varExprMapping.filter { case (k, v) => v.isInstanceOf[Id] }.map { case (k, v) => (k, v.asInstanceOf[Id]) }
    val substitution = utils.Substitution(mapping)
    substitution.apply(elem)
  }
}

/** Context for expression derivation.
  *
  * @param typeSystem
  *   The typesystem in use.
  * @param expression
  *   The expression which is typed currently.
  * @param gamma
  *   The hypermapping used to type the expression.
  * @param delta
  *   The deltamapping used to type the expression.
  * @param varMapping
  *   The mapping from captured variables to expressions.
  * @param cache
  *   The cache used to store already calculated derivations.
  */
case class ExpressionDerivationContext(val typeSystem: TypeSystem, val expression: Expr, val gamma: HyperMapping, val delta: DeltaMapping, val varMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def varExprMapping: Map[Id, Expr] = varMapping

  override def varStmtMapping: Map[Id, Stmt] = throw new Exception("ExpressionDerivationContext does not have a statement mapping")

  override def expr: Expr = expression

  override def statement: Stmt = throw new Exception("ExpressionDerivationContext does not have a statement")

  override def variables: ScalaSet[Id] = getVariables(expression)

}

/** Context for statement derivation.
  *
  * @param typeSystem
  *   The typesystem in use.
  * @param stmt
  *   The statement which is typed currently.
  * @param gamma
  *   The hypermapping used to type the statement.
  * @param delta
  *   The deltamapping used to type the statement.
  * @param varStmtMapping
  *   The mapping from captured variables to statements.
  * @param varExprMapping
  *   The mapping from captured variables to expressions.
  * @param cache
  *   The cache used to store already calculated derivations.
  */
case class StatementDerivationContext(val typeSystem: TypeSystem, val stmt: Stmt, val gamma: HyperMapping, val delta: DeltaMapping, val varStmtMapping: Map[Id, Stmt], val varExprMapping: Map[Id, Expr], var cache: Cache) extends Context {

  override def expr: Expr = throw new Exception("StatementDerivationContext does not have an expression")

  override def statement: Stmt = stmt

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id] // TODO

}

/** Generic trait to capture both expression and statement derivation results.
  */
trait DerivationResult {
  def getExpressionResult: ExpressionDerivationResult
  def getStatementResult: StatementDerivationResult
  def appliedRules: Seq[RuleName]
}

/** Result of expression derivation.
  *
  * @param hyperTypeCollection
  *   The collection of hypertypes of the expression.
  * @param deltaCollection
  *   The deltacollection of the expression.
  * @param appliedRules
  *   The rules that were applied to reach this result.
  */
case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection, val appliedRules: Seq[RuleName] = Seq.empty) extends DerivationResult {

  override def getExpressionResult: ExpressionDerivationResult = this

  override def getStatementResult: StatementDerivationResult = throw new Exception("ExpressionDerivationResult does not have a statement result")

}

/** Result of statement derivation.
  *
  * @param hyperTypeCollection
  *   The collection of hypertypes of the statement.
  * @param deltaCollection
  *   The deltacollection of the statement.
  * @param appliedRules
  *   The rules that were applied to reach this result.
  */
case class StatementDerivationResult(val hyperTypeMapping: HyperMapping, val deltaMapping: DeltaMapping, val appliedRules: Seq[RuleName] = Seq.empty) extends DerivationResult {
  override def getExpressionResult: ExpressionDerivationResult = throw new Exception("StatementDerivationResult does not have an expression result")

  override def getStatementResult: StatementDerivationResult = this

}
