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


trait Condition {
    def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean
    def deltaApplies(deltaCollection: DeltaCollection): Boolean
}
case class ElementOf(val hyperType: HyperType) extends Condition {
    override def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean = false
    override def deltaApplies(deltaCollection: DeltaCollection): Boolean = false
}
case class DeltaContains(val variable : Id, val hyperType: HyperType) extends Condition {
    override def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean = false
    override def deltaApplies(deltaCollection: DeltaCollection): Boolean = false
}
case class VarNotInDelta(val variable: Id) extends Condition {
    override def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean = false
    override def deltaApplies(deltaCollection: DeltaCollection): Boolean = false
}

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


abstract class BinaryExpressionDerivationRule extends ExpressionDerivationRule {
    val operator : ExpressionOperator
    val combineFunctionHypertype : binaryCombineFunction
    val combineFunctionDelta : binaryCombineFunction
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
                    case ElementOf(hyperType) => {
                        HyperTypes.semantic(hyperType, output)
                    }
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



case class binaryFunctionImplication(e1Hypertype : Seq[Condition], e1Delta : Seq[Condition], e2Hypertype : Seq[Condition], e2Delta : Seq[Condition], conclusion: Seq[Condition]) {}
case class unaryFunctionImplication(eHypertype : Seq[Condition], eDelta : Seq[Condition], conclusion: Seq[Condition]) {}

abstract class binaryCombineFunction  extends CombineFunction {
    val rules : Seq[binaryFunctionImplication]
    def filterApplies(e1HyperType : HyperTypeCollection, e2Delta: DeltaCollection, e2HyperType: HyperTypeCollection, e1Delta: DeltaCollection): Seq[Seq[Condition]] = {
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

abstract class unaryCombineFunction extends CombineFunction {
    val rules : Seq[unaryFunctionImplication]
}