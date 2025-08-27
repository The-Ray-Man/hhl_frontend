package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast._
import viper.HHLVerifier.typing.dsl.{AssignStmt => AssignStmtPattern, HavocStmt => HavocStmtPattern}
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping

trait Tree

case class EmptyTree()                                                                                                                                              extends Tree
case class ExprTree(gamma: HyperMapping, delta: DeltaMapping, expr: Expr, result: ExpressionDerivationResult, appliedRules: Seq[RuleName], premises: Seq[ExprTree]) extends Tree

case class StmtTree(gamma: HyperMapping, delta: DeltaMapping, stmt: Stmt, result: StatementDerivationResult, appliedRules: Seq[RuleName], premises: Seq[Tree]) extends Tree

case class RuleName(filePath: String, op: String, index: Int) {

  val shortFileName               = filePath.split("/").lastOption.getOrElse(filePath).split("\\.").headOption.getOrElse("unnamed")
  override def toString(): String = s"${shortFileName}_${op}_$index"
  def toStringLong(): String      = s"$filePath::$op::$index"
}

object RuleName {
  def empty: RuleName                                                                = RuleName("unnamed", "unnamed", -1)
  def create(filePath: String, derivationRule: DerivationRule, index: Int): RuleName = {
    val operatorName = humanReadableRuleName(derivationRule)
    RuleName(filePath, operatorName, index)
  }

  // -------- Human-readable rule names --------------------------------------
  def humanReadableRuleName(dr: DerivationRule): String = dr match {
    case er: ExpressionDerivationRule =>
      er.expr match {
        case BinaryExpr(_, op, _) => binaryOpLabel(op)
        case UnaryExpr(op, _)     => unaryOpLabel(op)
        case ImpliesExpr(_, _)    => "implies"
        case Id(name)             =>
          name match {
            case "var" => "variable"
            case "n"   => "num"
            case "b"   => "bool"
          }
        case LookupExpr(_, _)   => "lookup"
        case LengthExpr(_)      => "length"
        case CombExpr(_, _, op) => combOpLabel(op)
        case _                  => throw new Exception("No typing rules considered for this expression head.")
      }
    case stmt: StatementDerivationRule => {
      stmt.statement match {
        case AssignStmtPattern(_, _) => "assign"
        case CompStmt(_, _)          => "composition"
        case IfStmt(_, _, _)         => "ifElse"
        case HavocStmtPattern(_)     => "havoc"
        case InitStmt()              => "init"
        case MethodInitStmt(_)       => "methodInit"
        case _                       => throw new Exception("No typing rules considered for this statement head.")
      }
    }
  }

  def binaryOpLabel(op: String): String = op match {
    case "+"   => "plus"
    case "-"   => "minus"
    case "*"   => "times"
    case "/"   => "div"
    case "%"   => "modulo"
    case "<"   => "lessThan"
    case "<="  => "lessThanOrEqual"
    case ">"   => "greaterThan"
    case ">="  => "greaterThanOrEqual"
    case "=="  => "equal"
    case "!="  => "notEqual"
    case "&&"  => "and"
    case "||"  => "or"
    case "==>" => "implies"
  }

  def unaryOpLabel(op: String): String = op match {
    case "!" => "not"
    case "-" => "negate"
  }

  def combOpLabel(op: String): String = op match {
    case "++"           => "concat"
    case "setminus"     => "setminus"
    case "union"        => "union"
    case "in"           => "contains"
    case "intersection" => "intersection"
  }
}
