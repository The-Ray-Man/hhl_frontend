package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.HyperType
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing.IntType
import viper.HHLVerifier.typing.HyperTypes
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.rules.expression.BooleanDerivationRule
import viper.HHLVerifier.typing.rules.expression.NumericalDerivationRule
import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.ast.UnaryExpr





abstract class ExpressionDerivationRule {
    val combineFunctionHypertype: CombineFunction[HyperTypeConclusion]
    val combineFunctionDelta: CombineFunction[DeltaConclusion]

    def generateSoundnessTests: Seq[HHLProgram]
    def derive(system: ExpressionSystem, expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection)

    def applyBinary(system : ExpressionSystem, e: Expr, e1: Expr, e2: Expr, mapping: HyperMapping) : (HyperTypeCollection, DeltaCollection) = {
        val (type1, delta1) = system.derive(e1, mapping)
        val (type2, delta2) = system.derive(e2, mapping)

        val derivationContext = ExpressionDerivationContext(e, mapping, Seq(ExpressionDerivationResult(type1, delta1), ExpressionDerivationResult(type2, delta2)))
        val applicableRulesHypertypes = combineFunctionHypertype.getActions(derivationContext)
        val applicableRulesDeltas = combineFunctionDelta.getActions(derivationContext)

        var (hyperTypeCollection, deltaTypeCollection) = applicableRulesDeltas.foldLeft((HyperTypeCollection(), DeltaCollection(Map()))) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        val tmp = applicableRulesHypertypes.foldLeft((hyperTypeCollection, deltaTypeCollection)) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        hyperTypeCollection = tmp._1
        deltaTypeCollection = tmp._2
        (hyperTypeCollection, deltaTypeCollection)
    }

    def applyUnary(system : ExpressionSystem, e: Expr, e1: Expr, mapping: HyperMapping) : (HyperTypeCollection, DeltaCollection) = {
        val (type1, delta1) = system.derive(e1, mapping)

        val derivationContext = ExpressionDerivationContext(e, mapping, Seq(ExpressionDerivationResult(type1, delta1)))
        val applicableRulesHypertypes = combineFunctionHypertype.getActions(derivationContext)
        val applicableRulesDeltas = combineFunctionDelta.getActions(derivationContext)

        var (hyperTypeCollection, deltaTypeCollection) = applicableRulesDeltas.foldLeft((HyperTypeCollection(), DeltaCollection(Map()))) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        val tmp = applicableRulesHypertypes.foldLeft((hyperTypeCollection, deltaTypeCollection)) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        hyperTypeCollection = tmp._1
        deltaTypeCollection = tmp._2
        (hyperTypeCollection, deltaTypeCollection)
    }

    def applyNullary(system: ExpressionSystem, e: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
        val derivationContext = ExpressionDerivationContext(e, mapping, Seq())
        val applicableRulesHypertypes = combineFunctionHypertype.getActions(derivationContext)
        val applicableRulesDeltas = combineFunctionDelta.getActions(derivationContext)

        var (hyperTypeCollection, deltaTypeCollection) = applicableRulesDeltas.foldLeft((HyperTypeCollection(), DeltaCollection(Map()))) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        val tmp = applicableRulesHypertypes.foldLeft((hyperTypeCollection, deltaTypeCollection)) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        hyperTypeCollection = tmp._1
        deltaTypeCollection = tmp._2
        (hyperTypeCollection, deltaTypeCollection)
    }
}



case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection) {}
case class ExpressionDerivationContext(val expression: Expr, val mapping: HyperMapping, val premisses: Seq[ExpressionDerivationResult]) {}



trait Rule[C <: Conclusion] {}

case class binaryFunctionImplication[C <: Conclusion](e1Hypertype : Seq[HyperTypeCondition], e1Delta : Seq[DeltaCondition], e2Hypertype : Seq[HyperTypeCondition], e2Delta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}
case class unaryFunctionImplication[C <: Conclusion](eHypertype : Seq[HyperTypeCondition], eDelta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}
case class nullaryFunctionImplication[C <: Conclusion](sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}

case class RuleCheckContext(variables: Seq[Id]) {}



abstract class RuleWrapper[C <: Conclusion](rule: Rule[C]) {
    def getActions(context: ExpressionDerivationContext): Seq[Action] 
    def checkRules(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = {
        rule match {
            case binaryFunc@binaryFunctionImplication(_, _, _, _, _, _) => {
                val rule_result = binaryFunc.e1Hypertype.forall(cond => cond.hyperApplies(context, ruleCheckContext, context.premisses.head.hyperTypeCollection)) &&
                binaryFunc.e1Delta.forall(cond => cond.deltaApplies(context, ruleCheckContext, context.premisses.head.deltaCollection)) &&
                binaryFunc.e2Hypertype.forall(cond => cond.hyperApplies(context, ruleCheckContext, context.premisses(1).hyperTypeCollection)) &&
                binaryFunc.e2Delta.forall(cond => cond.deltaApplies(context, ruleCheckContext, context.premisses(1).deltaCollection)) &&
                binaryFunc.sideCondition.forall(cond => cond.applies(context, ruleCheckContext))

                if (rule_result) {
                    binaryFunc.conclusion.map(conclusion => conclusion.toActions(context, ruleCheckContext)).flatten
                } else {
                    Seq.empty[Action]
                }
            }
            case unaryFunc@unaryFunctionImplication(_, _, _, _) => {
                val rule_result = unaryFunc.eHypertype.forall(cond => cond.hyperApplies(context, ruleCheckContext, context.premisses.head.hyperTypeCollection)) &&
                unaryFunc.eDelta.forall(cond => cond.deltaApplies(context, ruleCheckContext, context.premisses.head.deltaCollection)) &&
                unaryFunc.sideCondition.forall(cond => cond.applies(context, ruleCheckContext))

                if (rule_result) {
                    unaryFunc.conclusion.map(conclusion => conclusion.toActions(context, ruleCheckContext)).flatten
                } else {
                    Seq.empty[Action]
                }
            }
            case nullaryFunc@nullaryFunctionImplication(_, _) => {
                val rule_result = nullaryFunc.sideCondition.forall(cond => cond.applies(context, ruleCheckContext))

                if (rule_result) {
                    nullaryFunc.conclusion.map(conclusion => conclusion.toActions(context, ruleCheckContext)).flatten
                } else {
                    Seq.empty[Action]
                }
            }
            case _ => throw new Exception("Unknown rule type: " + rule)
        }
    }
}

case class ForanyVariableWrapper[C <: Conclusion](rule: Rule[C]) extends RuleWrapper[C](rule: Rule[C]) {
    def getActions(context: ExpressionDerivationContext): Seq[Action] = {
        val variablesInExpression = getVariables(context.expression)
        val variablesInHyperMapping = context.mapping.mapping.keySet.map(Id(_))
        val allVariables = variablesInExpression ++ variablesInHyperMapping
        allVariables.map(id => {
            val ruleCheckContext = RuleCheckContext(Seq(id))
            checkRules(context, ruleCheckContext)
        }).toSeq.flatten
    }
}
case class EmptyWrapper[C <: Conclusion](rule : Rule[C]) extends RuleWrapper[C](rule : Rule[C]) {
    def getActions(context: ExpressionDerivationContext): Seq[Action] = {
        val ruleCheckContext = RuleCheckContext(Seq())
        checkRules(context, ruleCheckContext)
    }
}

abstract class CombineFunction[C <: Conclusion] {
    val rules : Seq[RuleWrapper[C]]
    def getActions(context : ExpressionDerivationContext): Seq[Action] = {
        rules.flatMap(rule => rule.getActions(context))
    }
}


abstract class binaryCombineFunction[C <: Conclusion]  extends CombineFunction[C] {}

abstract class unaryCombineFunction[C <: Conclusion] extends CombineFunction[C] {}

abstract class nullaryCombineFunction[C <: Conclusion] extends CombineFunction[C] {}