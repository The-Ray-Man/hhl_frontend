package viper.HHLVerifier.test

import viper.HHLVerifier.typing.HyperType
import viper.HHLVerifier.typing.{Neg, Low, Pos, Zero, One}
import viper.HHLVerifier.typing.GreaterOne
import viper.HHLVerifier.typing.LessOne
import viper.HHLVerifier.typing.MonoUp
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.MonoDown
import java.io.PrintWriter
import viper.HHLVerifier.parsing.Parser
import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.typing.HyperTranslate
import viper.HHLVerifier.symbols.SymbolChecker
import viper.HHLVerifier.typing.TypeChecker
import viper.HHLVerifier.generation.Generator
import viper.HHLVerifier.management.ViperRunner
import viper.silver.verifier.Success
import viper.silver.verifier.Failure 
import viper.silicon.state.terms.Fun
import viper.HHLVerifier.test.ExpressionOperator.Add
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.BinaryExpr
import upack.Binary
import viper.HHLVerifier.typing.HyperTypes
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.typing.IntType
import org.checkerframework.checker.units.qual.s


trait Condition {}
case class ElementOf(val hyperType: HyperType) extends Condition
case class DeltaContains(val variable : Id, val hyperType: HyperType) extends Condition
case class VarNotInDelta(val variable: Id) extends Condition


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


abstract class ExpressionDerivationRule {
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

abstract class binaryCombineFunction {
    val rules : Seq[binaryFunctionImplication]
}

abstract class unaryCombineFunction {
    val rules : Seq[unaryFunctionImplication]
}



object RuleSoundnessTests {

    def runTest(program: HHLProgram) : Unit = {
        SymbolChecker.reset()
        TypeChecker.reset()
        Generator.reset()


        HyperTypeChecker.typeCheckProg(program)
        println("hypertypecheck done")
        val programTranslated = HyperTranslate.translateProgram(program)
        println("hypertranslate done")
        SymbolChecker.checkSymbolsProg(programTranslated)
        println("symbol check done")
        TypeChecker.typeCheckProg(programTranslated)
        println("type check done")
        val viperProgram = Generator.generate(programTranslated, "")
        val consistencyErrors = viperProgram.checkTransitively
        if (consistencyErrors.isEmpty) {
            val result = ViperRunner.runSiliconAndCarbon(viperProgram)
            result match {
                case Success =>
                    println(s"Test passed successfully.")
                case Failure(errors) =>
                    throw new RuntimeException(s"Test failed with errors: ${errors.mkString(", ")}")
            }
        } else {
            throw new RuntimeException(s"Test failed with errors: ${consistencyErrors.mkString(", ")}")
        }
    }

    def testExpressionRule(rule: ExpressionDerivationRule) =  {
        val tests = rule.generateSoundnessTests
        var successTests = Seq.empty[String]
        var failureTests = Seq.empty[String]
        for ((test, id) <- tests.zipWithIndex) {
            println(s"Running test $id for rule ${rule.operator}")
            try {
                runTest(test)
                successTests :+= s"Test $id for rule ${rule.operator} passed."
            } catch {
                case e: Exception =>
                    println(s"Test $id for rule ${rule.operator} failed with exception: ${e.getMessage}")
                    failureTests :+= s"Test $id for rule ${rule.operator} failed."
            }
        }

        println(s"Total tests ${tests.size} for rule ${rule.operator}.")
        println(s"Successful tests: ${successTests.size}")
        println(s"Failed tests: ${failureTests.size}")
        if (failureTests.nonEmpty) {
            println("Failed tests:")
            failureTests.foreach(println)
        }
    }


    def main() : Unit = {
        testExpressionRule(rules.AdditionDerivationRule())
    }
}