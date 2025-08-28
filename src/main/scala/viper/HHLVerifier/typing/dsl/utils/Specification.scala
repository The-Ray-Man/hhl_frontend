package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.typing.dsl.ast.{ExpressionDerivationRule, Specification, DerivationRule, StatementDerivationRule}
import viper.HHLVerifier.ast.{Id, BinaryExpr, ImpliesExpr, LengthExpr, LookupExpr, UnaryExpr}
import viper.HHLVerifier.typing.dsl.ast.{AssignStmt, IfStmt, CompStmt}

object SpecificationUtil {

  def combineSpecifications(specifications: Seq[Specification]): Specification = {

    val specificationCombined = specifications.reduce((acc, spec) => {
      SpecificationUtil.mergeSpecification(acc, spec)
    })
    specificationCombined
  }

  def mergeSpecification(spec1: Specification, spec2: Specification): Specification = {
    val hyperTypeDeclarations = spec1.hypertypeDeclaration ++ spec2.hypertypeDeclaration

    val derivationRules = spec1.derivationRules ++ spec2.derivationRules
    val reducedRules    = combineDerivationRules(derivationRules)

    Specification(
      hyperTypeDeclarations,
      reducedRules
    )
  }

  def combineDerivationRules(rules: Seq[DerivationRule]): Seq[DerivationRule] = {
    var expressionRules = Set[ExpressionDerivationRule]()
    var statementRules  = Set[StatementDerivationRule]()
    for (rule <- rules) {
      if (rule.isInstanceOf[ExpressionDerivationRule]) {
        val dupplicatedRule = expressionRules.find(r => canBeCombined(r, rule.asInstanceOf[ExpressionDerivationRule]).isDefined)
        dupplicatedRule match {
          case None        => expressionRules += rule.asInstanceOf[ExpressionDerivationRule]
          case Some(value) => {
            val combinedRule = combineExpressionDerivationRules(value, rule.asInstanceOf[ExpressionDerivationRule]).getOrElse(throw new Exception("This should never happen"))
            expressionRules -= value
            expressionRules += combinedRule
          }
        }
      } else {
        val dupplicatedRule = statementRules.find(r => canBeCombined(r, rule.asInstanceOf[StatementDerivationRule]).isDefined)
        dupplicatedRule match {
          case None        => statementRules += rule.asInstanceOf[StatementDerivationRule]
          case Some(value) => {
            val combinedRule = combineStatementDerivationRules(value, rule.asInstanceOf[StatementDerivationRule]).getOrElse(throw new Exception("This should never happen"))
            statementRules -= value
            statementRules += combinedRule
          }
        }
      }
    }
    expressionRules.toSeq ++ statementRules.toSeq
  }

  def combineExpressionDerivationRules(rule1: ExpressionDerivationRule, rule2: ExpressionDerivationRule): Option[ExpressionDerivationRule] = {
    canBeCombined(rule1, rule2) match {
      case None                => None
      case Some(mappingIdtoId) => {
        val adaptedRules = rule2.rules.map(rule => Substitution.apply(mappingIdtoId, rule))
        val allRules     = (rule1.rules ++ adaptedRules).toSet.toSeq
        Some(ExpressionDerivationRule(rule1.expr, allRules))
      }
    }
  }

  def combineStatementDerivationRules(rule1: StatementDerivationRule, rule2: StatementDerivationRule): Option[StatementDerivationRule] = {
    canBeCombined(rule1, rule2) match {
      case None                => None
      case Some(mappingIdtoId) => {
        val adaptedRules = rule2.rules.map(rule => Substitution.apply(mappingIdtoId, rule))
        val allRules     = (rule1.rules ++ adaptedRules).toSet.toSeq
        Some(StatementDerivationRule(rule1.statement, allRules))
      }
    }
  }

  def canBeCombined(rule1: ExpressionDerivationRule, rule2: ExpressionDerivationRule): Option[Map[Id, Id]] = {
    (rule1.expr, rule2.expr) match {
      case (Id(name1), Id(name2)) if name1 == name2                                                                               => Some(Map(Id(name1) -> Id(name2)))
      case (BinaryExpr(idLeft1 @ Id(_), op1, idRight1 @ Id(_)), BinaryExpr(idLeft2 @ Id(_), op2, idRight2 @ Id(_))) if op1 == op2 => Some(Map(idLeft1 -> idLeft2, idRight1 -> idRight2))
      case (UnaryExpr(op1, idInner1 @ Id(_)), UnaryExpr(op2, idInner2 @ Id(_))) if op1 == op2                                     => Some(Map(idInner1 -> idInner2))
      case (ImpliesExpr(idLeft1 @ Id(_), idRight1 @ Id(_)), ImpliesExpr(idLeft2 @ Id(_), idRight2 @ Id(_)))                       => Some(Map(idLeft1 -> idLeft2, idRight1 -> idRight2))
      case (LengthExpr(id1 @ Id(_)), LengthExpr(id2 @ Id(_))) if id1 == id2                                                       => Some(Map(id1 -> id2))
      case (LookupExpr(dataStructure1 @ Id(_), index1 @ Id(_)), LookupExpr(dataStructure2 @ Id(_), index2 @ Id(_)))               => Some(Map(dataStructure1 -> dataStructure2, index1 -> index2))
      case _                                                                                                                      => None
    }
  }

  def canBeCombined(rule1: StatementDerivationRule, rule2: StatementDerivationRule): Option[Map[Id, Id]] = {
    (rule1.statement, rule2.statement) match {
      case (AssignStmt(var1, value1), AssignStmt(var2, value2))                               => Some(Map(var1 -> var2, value1 -> value2))
      case (IfStmt(cond1, thenBranch1, elseBranch1), IfStmt(cond2, thenBranch2, elseBranch2)) =>
        Some(Map(cond1 -> cond2, thenBranch1 -> thenBranch2, elseBranch1 -> elseBranch2))
      case (CompStmt(firstStmt1, secondStmt1), CompStmt(firstStmt2, secondStmt2)) =>
        Some(Map(firstStmt1 -> firstStmt2, secondStmt1 -> secondStmt2))
      case _ => None
    }
  }

}
