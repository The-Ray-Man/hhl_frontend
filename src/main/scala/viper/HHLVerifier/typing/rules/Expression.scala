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

trait CombineFunction {}

trait ExpressionDerivationRule {
    val operator: ExpressionOperator
    val combineFunctionHypertype: CombineFunction
    val combineFunctionDelta: CombineFunction
    def generateSoundnessTests: Seq[HHLProgram]
}

object ExpressionDerivationRule {
    def derive(e: Expr, mapping: HyperMapping) : (HyperTypeCollection, DeltaCollection) = {
        e match {
            case BinaryExpr(e1, op, e2) => {
                op match {
                    case "+" => AdditionDerivationRule().derive(e1, e2, mapping)
                    case _ => throw new Exception("Unknown binary operator: " + op)
                }
            }
            case _ => throw new Exception("Cannot derive expression: " + e)
        }
    }
}


abstract class BinaryExpressionDerivationRule extends ExpressionDerivationRule {
    val operator : ExpressionOperator
    val combineFunctionHypertype : binaryCombineFunction[HyperTypeConclusion]
    val combineFunctionDelta : binaryCombineFunction[DeltaConclusion]

    def derive(e1: Expr, e2: Expr, mapping: HyperMapping): (HyperTypeCollection, DeltaCollection) = {
        val (type1, delta1) = ExpressionDerivationRule.derive(e1, mapping)
        val (type2, delta2) = ExpressionDerivationRule.derive(e2, mapping)

        val applicableRulesHypertypes = combineFunctionHypertype.filterApplies(type1, delta1, type2, delta2)

        val applicableRulesDeltas = combineFunctionDelta.filterApplies(type1, delta1, type2, delta2)

        var (hyperTypeCollection, deltaTypeCollection) = applicableRulesDeltas.flatten.foldLeft((HyperTypeCollection(), DeltaCollection(Map()))) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        val tmp = applicableRulesHypertypes.flatten.foldLeft((hyperTypeCollection, deltaTypeCollection)) { (acc, rule) =>
            rule.apply(acc._1, acc._2)
        }

        hyperTypeCollection = tmp._1
        deltaTypeCollection = tmp._2
        (hyperTypeCollection, deltaTypeCollection)
    }

    def generateSoundnessTests : Seq[HHLProgram] = {

        var testPrograms = Seq.empty[HHLProgram]
        for (rule <- combineFunctionHypertype.rules) {
            val e1 = Id("e1")
            e1.typ = IntType()

            val e2 = Id("e2")
            e2.typ = IntType()

            val monoId = Id("monoId")
            monoId.typ = IntType()

            val output = Id("output")
            output.typ = IntType()


            val preconditionsE1 = rule.e1Hypertype.map(cond => {
                cond match {
                    case ElementOf(hyperType) => {
                        HyperTypes.semantic(hyperType, e1)
                    }
                    case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
                    
                }
            })
            val preconditionsE2 = rule.e2Hypertype.map(cond => {
                cond match {
                    case ElementOf(hyperType) => {
                        HyperTypes.semantic(hyperType, e2)
                    }
                    case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
                    
                }
            })

            val preconditions = preconditionsE1 ++ preconditionsE2

            val conclusion = rule.conclusion.map(cond => {
                cond match {
                    // case ElementOf(hyperType) => {
                    //     HyperTypes.semantic(hyperType, output)
                    // }
                    case _ => throw new IllegalArgumentException(s"Unknown condition: $cond")
                }
            })

            val program = HHLProgram(Seq(
                Method("test", Seq(e1, e2, monoId), Seq(output), preconditions, conclusion, CompositeStmt(Seq(AssignStmt(output, operator.expression(e1,e2)))))
            ))

            testPrograms :+= program

        }

        testPrograms
    }
}



case class binaryFunctionImplication[C <: Conclusion](e1Hypertype : Seq[HyperTypeCondition], e1Delta : Seq[DeltaCondition], e2Hypertype : Seq[HyperTypeCondition], e2Delta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) {}
case class unaryFunctionImplication[C <: Conclusion](eHypertype : Seq[HyperTypeCondition], eDelta : Seq[DeltaCondition], sideCondition: Seq[SideCondition], conclusion: Seq[C]) {}
case class nullaryFunctionImplication[C <: Conclusion](sideCondition: Seq[SideCondition], conclusion: Seq[C]) {}

abstract class binaryCombineFunction[C <: Conclusion]  extends CombineFunction {
    val rules : Seq[binaryFunctionImplication[C]]
    def filterApplies(e1HyperType : HyperTypeCollection, e2Delta: DeltaCollection, e2HyperType: HyperTypeCollection, e1Delta: DeltaCollection): Seq[Seq[Conclusion]] = {
        rules.filter(rule => {
            rule.e1Hypertype.forall(cond => cond.hyperApplies(e1HyperType)) &&
            rule.e1Delta.forall(cond => cond.deltaApplies(e1Delta)) &&
            rule.e2Hypertype.forall(cond => cond.hyperApplies(e2HyperType)) &&
            rule.e2Delta.forall(cond => cond.deltaApplies(e2Delta))
        }).map(rule => {
            rule.conclusion
        })
    }
}

abstract class unaryCombineFunction[C <: Conclusion] extends CombineFunction {
    val rules : Seq[unaryFunctionImplication[C]]
    def filterApplies(hyperType : HyperTypeCollection, delta: DeltaCollection): Seq[Seq[Conclusion]] = {
        rules.filter(rule => {
            rule.eHypertype.forall(cond => cond.hyperApplies(hyperType)) &&
            rule.eDelta.forall(cond => cond.deltaApplies(delta))
        }).map(rule => {
            rule.conclusion
        })
    }
}

abstract class nullaryCombineFunction[C <: Conclusion] extends CombineFunction {
    val rules : Seq[nullaryFunctionImplication[C]]
    def filterApplies(context: Any): Seq[Seq[Conclusion]] = {
        rules.filter(rule => {
            rule.sideCondition.forall(cond => cond.applies(context))
        }).map(rule => {
            rule.conclusion
        })
    }
}