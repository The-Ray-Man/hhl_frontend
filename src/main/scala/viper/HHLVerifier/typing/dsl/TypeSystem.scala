package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl.SpecificationUtil
import viper.HHLVerifier.typing.dsl.HyperTypeDeclaration
import viper.HHLVerifier.typing.dsl
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.dsl.ExpressionDerivationResult
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.FrameStmt
import viper.HHLVerifier.ast.HyperAssertStmt
import viper.HHLVerifier.ast
import viper.HHLVerifier.ast.DeclareStmt
import viper.HHLVerifier.ast.ProofVarDecl
import viper.HHLVerifier.ast.MultiAssignStmt
import viper.HHLVerifier.ast.MethodCallStmt
import viper.HHLVerifier.ast.UseHintStmt
import viper.HHLVerifier.ast.WhileLoopStmt
import viper.HHLVerifier.ast.HyperAssumeStmt
import viper.HHLVerifier.ast.PVarDecl
import viper.HHLVerifier.ast.AssumeStmt
import viper.HHLVerifier.ast.IfElseStmt
import viper.HHLVerifier.ast.HavocStmt
import viper.HHLVerifier.ast.ReuseStmt
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.UnfoldStmt
import viper.HHLVerifier.ast.FoldStmt
import viper.HHLVerifier.ast.AssertStmt

case class TypeSystem(
    statementTypeSystem: StatementTypeSystem = null,
    expressionTypeSystem: Seq[dsl.ExpressionDerivationRule] = null,
    hyperTypeDeclaration: Seq[HyperTypeDeclaration] = Seq.empty[HyperTypeDeclaration]
) {

  def checkSoundness(): Boolean = {
    false
  }

  def deriveExpression(gamma: HyperMapping, delta: DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): ExpressionDerivationResult = {
    println("derivingExpression: " + expr)
    val applicableRules = expressionTypeSystem.map(rule => (rule, rule.isApplicableTo(expr))).filter(_._2.isDefined).map(rule => (rule._1, rule._2.get))
    if (applicableRules.isEmpty || applicableRules.length > 1) {
      throw new Exception(s"There are ${applicableRules.length} applicable rules for expression $expr")
    }
    val rule = applicableRules.head
    rule._1.derive(this, gamma, delta, expr, rule._2)
  }

  def deriveStatement(gamma: HyperMapping, delta: DeltaMapping, s: Stmt, pc: HyperTypeCollection): StatementDerivationResult = {
    println("derivingStatement: " + s)
    s match {
      case ast.AssignStmt(left, right) => {
        val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.assignRule.statement).getOrElse(throw new Exception(s"Statement $s does not match assign pattern"))
        val context                      = StatementDerivationContext(this, s, gamma, delta, pc, stmtMatching, exprMatching)
        statementTypeSystem.assignRule.derive(context)
      }
      case CompositeStmt(stmts) => {
        if (stmts.length == 0 ) {
          StatementDerivationResult(gamma, delta)
        }
        else if (stmts.length == 1) {
          deriveStatement(gamma, delta, stmts.head, pc)
        } else {
          val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.compositionRule.statement).getOrElse(throw new Exception(s"Statement $s does not match assign pattern"))
          val context                      = StatementDerivationContext(this, s, gamma, delta, pc, stmtMatching, exprMatching)
          statementTypeSystem.compositionRule.derive(context)
        }
      }
      case IfElseStmt(cond, ifStmt, elseStmt) => {
        val (stmtMatching, exprMatching) = statementMatchesPattern(s, statementTypeSystem.branchRule.statement).getOrElse(throw new Exception(s"Statement $s does not match branch pattern"))
        val context                      = StatementDerivationContext(this, s, gamma, delta, pc, stmtMatching, exprMatching)
        statementTypeSystem.branchRule.derive(context)
      }
      case WhileLoopStmt(cond, body, _, _, _)  => {
        var currentGamma = gamma
        var currentDelta = delta
        var newGamma = gamma
        var newDelta = delta
        do {
          currentGamma = newGamma
          currentDelta = newDelta
          val condResult = deriveStatement(currentGamma, currentDelta, IfElseStmt(cond, body, CompositeStmt(Seq())), pc)
          newGamma = condResult.getStatementResult.hyperTypeMapping
          newDelta = condResult.getStatementResult.deltaMapping
        } while (newGamma != gamma || newDelta != delta)
        StatementDerivationResult(
          hyperTypeMapping = newGamma,
          deltaMapping = newDelta
        )
      }
      case MultiAssignStmt(_, _)                                                                                                                                                                                 => throw new Exception("MultiAssignStmt is not yet supported in the type system")
      case UnfoldStmt(_, _)                                                                                                                                                                                      => throw new Exception("Fold statements are not yet supported in the type system")
      case FoldStmt(_, _)                                                                                                                                                                                        => throw new Exception("Fold statements are not yet supported in the type system")
      case MethodCallStmt(_, _)                                                                                                                                                                                  => throw new Exception("Method calls are not yet supported in the type system")
      case AssumeStmt(_) | UseHintStmt(_) | HyperAssumeStmt(_) | PVarDecl(_, _) | DeclareStmt(_, _) | HyperAssertStmt(_) | ProofVarDecl(_, _) | ReuseStmt(_) | HavocStmt(_, _) | AssertStmt(_) | FrameStmt(_, _) => StatementDerivationResult(gamma, delta)
    }
  }

  def statementMatchesPattern(stmt: Stmt, pattern: StmtPattern): Option[(Map[Id, Stmt], Map[Id, Expr])] = {
    (pattern, stmt) match {
      case (AssignStmt(left, right), ast.AssignStmt(leftCheck, rightCheck)) =>
        Some((Map.empty, Map(left -> leftCheck, right -> rightCheck)))
      case (IfStmt(cond, ifStmt, elseStmt), IfElseStmt(condCheck, ifStmtCheck, elseStmtCheck)) =>
        Some(Map(ifStmt -> ifStmtCheck, elseStmt -> elseStmtCheck), Map(cond -> condCheck))
      case (CompStmt(s1, s2), CompositeStmt(stmtsCheck)) => {
        if (stmtsCheck.length == 1) {
          throw new Exception("CompositeStmt pattern matching is not supported for single statements")
        } else if (stmtsCheck.length == 2) {
          Some(Map(s1 -> stmtsCheck.head, s2 -> stmtsCheck(1)), Map.empty)
        } else {
          Some(Map(s1 -> stmtsCheck.head, s2 -> CompositeStmt(stmtsCheck.tail)), Map.empty)
        }
      }
      case _ => None
    }
  }
}

case class StatementTypeSystem(assignRule: StatementDerivationRule, compositionRule: StatementDerivationRule, branchRule: StatementDerivationRule) {}

object TypeSystem {
  def loadTypeSystem(paths: Seq[String]): TypeSystem = {
    val specifications = paths.map(path => {
      val fileContent = scala.io.Source.fromFile(path).getLines().mkString("\n")
      val res         = fastparse.parse(fileContent, viper.HHLVerifier.typing.dsl.Parser.specification(_))
      res match {
        case fastparse.Parsed.Success(value, _) => value
        case failure: fastparse.Parsed.Failure  => throw new Exception(s"Failed to parse type system: ${failure.msg}")
      }
    })

    SpecificationUtil.combineSpecifications(specifications).toTypeSystem()
  }
}
