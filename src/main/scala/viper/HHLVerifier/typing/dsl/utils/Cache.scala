package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.ast.{Expr, Stmt}
import viper.HHLVerifier.typing.dsl.{HyperMapping, DeltaMapping, ExpressionDerivationResult, StatementDerivationResult}

class Cache() {
  var expressionCache: Map[(Expr, HyperMapping, DeltaMapping), ExpressionDerivationResult] = Map()
  var statementCache: Map[(Stmt, HyperMapping, DeltaMapping), StatementDerivationResult]   = Map()

  def add(gamma: HyperMapping, delta: DeltaMapping, expr: Expr, result: ExpressionDerivationResult): Unit = {
    expressionCache += ((expr, gamma, delta) -> result)
  }
  def add(gamma: HyperMapping, delta: DeltaMapping, stmt: Stmt, result: StatementDerivationResult): Unit = {
    statementCache += ((stmt, gamma, delta) -> result)
  }

  def get(gamma: HyperMapping, delta: DeltaMapping, expr: Expr): Option[ExpressionDerivationResult] = {
    expressionCache.get((expr, gamma, delta))
  }

  def get(gamma: HyperMapping, delta: DeltaMapping, stmt: Stmt): Option[StatementDerivationResult] = {
    statementCache.get((stmt, gamma, delta))
  }
}
