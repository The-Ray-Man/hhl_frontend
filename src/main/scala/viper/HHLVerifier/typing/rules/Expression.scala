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
import viper.HHLVerifier.typing.rules.expression.AdditionDerivationRule
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.rules.expression.BooleanDerivationRule
import viper.HHLVerifier.typing.rules.expression.NumericalDerivationRule
import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule





sealed trait ExpressionOperator {
    def expression(e1: Expr, e2: Expr): Expr
}

object ExpressionOperator {
    case object Add extends ExpressionOperator {
      override def expression(e1: Expr, e2: Expr): Expr = BinaryExpr(e1, "+" ,e2)
    }
    case object Subtract extends ExpressionOperator {
      override def expression(e1: Expr, e2: Expr): Expr = BinaryExpr(e1, "-" ,e2)
    }
    case object Multiply extends ExpressionOperator {
      override def expression(e1: Expr, e2: Expr): Expr = BinaryExpr(e1, "*" ,e2)
    }
    case object Divide extends ExpressionOperator {
      override def expression(e1: Expr, e2: Expr): Expr = BinaryExpr(e1, "/" ,e2)
    }
}



trait ExpressionDerivationRule {
    val operator: ExpressionOperator
    val combineFunctionHypertype: CombineFunction[HyperTypeConclusion]
    val combineFunctionDelta: CombineFunction[DeltaConclusion]
    def generateSoundnessTests: Seq[HHLProgram]
}

object ExpressionDerivationRule {
    def derive(e: Expr, mapping: HyperMapping) : (HyperTypeCollection, DeltaCollection) = {
        e match {
            case binaryExpr@BinaryExpr(_, op, _) => {
                op match {
                    case "+" => AdditionDerivationRule().derive(binaryExpr, mapping)
                    case _ => throw new Exception("Unknown binary operator: " + op)
                }
            }
            case BoolLit(_) => BooleanDerivationRule().derive(e, mapping)
            case Num(_) => NumericalDerivationRule().derive(e, mapping)
            case Id(_) => IdentifierDerivationRule().derive(e, mapping)
            case _ => throw new Exception("Cannot derive expression: " + e)
        }
    }
}

case class ExpressionDerivationResult(val hyperTypeCollection: HyperTypeCollection, val deltaCollection: DeltaCollection) {}
case class ExpressionDerivationContext(val expression: Expr, val mapping: HyperMapping, val premisses: Seq[ExpressionDerivationResult]) {}


abstract class BinaryExpressionDerivationRule extends ExpressionDerivationRule {
    val operator : ExpressionOperator
    val combineFunctionHypertype : binaryCombineFunction[HyperTypeConclusion]
    val combineFunctionDelta : binaryCombineFunction[DeltaConclusion]

    def derive(expression : BinaryExpr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
        val (type1, delta1) = ExpressionDerivationRule.derive(expression.e1, mapping)
        val (type2, delta2) = ExpressionDerivationRule.derive(expression.e2, mapping)

        val derivationContext = ExpressionDerivationContext(expression, mapping, Seq(ExpressionDerivationResult(type1, delta1), ExpressionDerivationResult(type2, delta2)))

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

    def generateSoundnessTests : Seq[HHLProgram] = {

        // var testPrograms = Seq.empty[HHLProgram]
        // for (rule <- combineFunctionHypertype.rules) {
        //     val e1 = Id("e1")
        //     e1.typ = IntType()

        //     val e2 = Id("e2")
        //     e2.typ = IntType()

        //     val monoId = Id("monoId")
        //     monoId.typ = IntType()

        //     val output = Id("output")
        //     output.typ = IntType()


        //     val preconditionsE1 = rule.e1Hypertype.map(cond => {
        //         cond match {
        //             case ElementOf(hyperType) => {
        //                 HyperTypes.semantic(hyperType, e1)
        //             }
        //             case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
                    
        //         }
        //     })
        //     val preconditionsE2 = rule.e2Hypertype.map(cond => {
        //         cond match {
        //             case ElementOf(hyperType) => {
        //                 HyperTypes.semantic(hyperType, e2)
        //             }
        //             case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
                    
        //         }
        //     })

        //     val preconditions = preconditionsE1 ++ preconditionsE2

        //     val conclusion = rule.conclusion.map(cond => {
        //         cond match {
        //             // case ElementOf(hyperType) => {
        //             //     HyperTypes.semantic(hyperType, output)
        //             // }
        //             case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
        //         }
        //     })

        //     val program = HHLProgram(Seq(
        //         Method("test", Seq(e1, e2, monoId), Seq(output), preconditions, conclusion, CompositeStmt(Seq(AssignStmt(output, operator.expression(e1,e2)))))
        //     ))

        //     testPrograms :+= program

        // }

        // testPrograms
        Seq.empty[HHLProgram] // TODO: Implement soundness tests
    }
}


abstract class NullaryExpressionDerivationRule extends ExpressionDerivationRule {
    val combineFunctionHypertype: nullaryCombineFunction[HyperTypeConclusion]
    val combineFunctionDelta: nullaryCombineFunction[DeltaConclusion]

    def derive(expression: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {

        val derivationContext = ExpressionDerivationContext(expression, mapping, Seq())

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

    def generateSoundnessTests: Seq[HHLProgram] = {
        Seq.empty[HHLProgram] // TODO
    }

}

trait Rule[C <: Conclusion] {}

case class binaryFunctionImplication[C <: Conclusion](e1Hypertype : Seq[HyperTypeCondition], e1Delta : Seq[DeltaCondition], e2Hypertype : Seq[HyperTypeCondition], e2Delta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}
case class unaryFunctionImplication[C <: Conclusion](eHypertype : Seq[HyperTypeCondition], eDelta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}
case class nullaryFunctionImplication[C <: Conclusion](sideCondition: Seq[SideCondition], conclusion: Seq[C]) extends Rule[C] {}

case class RuleCheckContext(variables: Seq[Id]) {}

trait RuleWrapper[C <: Conclusion] {
    def getActions(context: ExpressionDerivationContext): Seq[Action]
}

case class ApplyForEveryVariable[C <: Conclusion](rule : Rule[C]) extends RuleWrapper[C] {
    def getActions(context: ExpressionDerivationContext): Seq[Action] = {
        throw new Exception("ApplyForEveryVariable is not implemented yet")
    }
}
case class EmptyWrapper[C <: Conclusion](rule : Rule[C]) extends RuleWrapper[C] {
    def getActions(context: ExpressionDerivationContext): Seq[Action] = {
        val ruleCheckContext = RuleCheckContext(Seq())
        rule match {
            case binaryFunc@binaryFunctionImplication(_, _, _, _, _, _) => {
                val rule_result = binaryFunc.e1Hypertype.forall(cond => cond.hyperApplies(context.premisses.head.hyperTypeCollection)) &&
                binaryFunc.e1Delta.forall(cond => cond.deltaApplies(context.premisses.head.deltaCollection)) &&
                binaryFunc.e2Hypertype.forall(cond => cond.hyperApplies(context.premisses(1).hyperTypeCollection)) &&
                binaryFunc.e2Delta.forall(cond => cond.deltaApplies(context.premisses(1).deltaCollection)) &&
                binaryFunc.sideCondition.forall(cond => cond.applies(context))

                if (rule_result) {
                    binaryFunc.conclusion.map(conclusion => conclusion.toActions(context, ruleCheckContext)).flatten
                } else {
                    Seq.empty[Action]
                }
            }
            case unaryFunc@unaryFunctionImplication(_, _, _, _) => {
                val rule_result = unaryFunc.eHypertype.forall(cond => cond.hyperApplies(context.premisses.head.hyperTypeCollection)) &&
                unaryFunc.eDelta.forall(cond => cond.deltaApplies(context.premisses.head.deltaCollection)) &&
                unaryFunc.sideCondition.forall(cond => cond.applies(context))

                if (rule_result) {
                    unaryFunc.conclusion.map(conclusion => conclusion.toActions(context, ruleCheckContext)).flatten
                } else {
                    Seq.empty[Action]
                }
            }
            case nullaryFunc@nullaryFunctionImplication(_, _) => {
                val rule_result = nullaryFunc.sideCondition.forall(cond => cond.applies(context))

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

abstract class CombineFunction[C <: Conclusion] {
    val rules : Seq[RuleWrapper[C]]
    def getActions(context : ExpressionDerivationContext): Seq[Action] = {
        rules.flatMap(rule => rule.getActions(context))
    }
}


abstract class binaryCombineFunction[C <: Conclusion]  extends CombineFunction[C] {}

abstract class unaryCombineFunction[C <: Conclusion] extends CombineFunction[C] {}

abstract class nullaryCombineFunction[C <: Conclusion] extends CombineFunction[C] {}