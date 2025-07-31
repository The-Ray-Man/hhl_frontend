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
            val e1prime = Id("e1prime")
            e1prime.typ = IntType()
            val e2 = Id("e2")
            e2.typ = IntType()
            val e2prime = Id("e2prime")
            e2prime.typ = IntType()

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
                Method("test", Seq(e1,e1prime, e2, e2prime, monoId), Seq(output), preconditions, conclusion, CompositeStmt(Seq(AssignStmt(output, operator.expression(e1,e2)))))
            ))
            println(program)

            testPrograms :+= program

        }

        testPrograms
    }
}



case class binaryFunctionImplication(e1Hypertype : Seq[Condition], e1Delta : Seq[Condition], e2Hypertype : Seq[Condition], e2Delta : Seq[Condition], conclusion: Seq[Condition]) {}


abstract class binaryCombineFunction {
    val rules : Seq[binaryFunctionImplication]
}

case class AdditionCombineFunctionHypertype() extends binaryCombineFunction {
    val rules = Seq(
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(Low()))),
        // binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(), Seq(), Seq(ElementOf(Low()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Pos()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(Pos()), ElementOf(LessOne())), Seq(), Seq(ElementOf(Neg()), ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Neg()))),
        binaryFunctionImplication(Seq(ElementOf(MonoUp(Set(Id("monoId"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId"))))), Seq(), Seq(ElementOf(MonoUp(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(MonoDown(Set(Id("monoId"))))), Seq(), Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(Low())), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId"))))), Seq(), Seq(ElementOf(MonoDown(Set(Id("monoId")))))),
        binaryFunctionImplication(Seq(ElementOf(One())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(One()))), 
        binaryFunctionImplication(Seq(ElementOf(GreaterOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(LessOne())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(One())), Seq(), Seq(ElementOf(One()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(LessOne())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(Zero())), Seq(), Seq(ElementOf(GreaterOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(GreaterOne()))),
        binaryFunctionImplication(Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Neg())), Seq(), Seq(ElementOf(LessOne()))),
        binaryFunctionImplication(Seq(ElementOf(Neg()), ElementOf(LessOne())), Seq(), Seq(ElementOf(LessOne()), ElementOf(Pos())), Seq(), Seq(ElementOf(LessOne()))),
    )
}


case class AdditionCombineFunctionDeltatype() extends binaryCombineFunction {
    val rules = Seq()
}


case class AdditionDerivationRule() extends ExpressionDerivationRule {

  override val operator: ExpressionOperator = ExpressionOperator.Add

  override val combineFunctionHypertype: binaryCombineFunction = AdditionCombineFunctionHypertype()

  override val combineFunctionDelta: binaryCombineFunction = AdditionCombineFunctionDeltatype()

}



object TestGeneratorHelper {
    def stringToHyperType(s: String) : HyperType = {
        s match {
            case "low" => Low()
            case "pos" => Pos()
            case "zero" => Zero()
            case "negative" => Neg()
            case "absGtOne" => GreaterOne()
            case "absLtOne" => LessOne()
            case "absOne" => One()
            case "mono{id}{uparrow}" => MonoUp(Set(Id("id")))
            case "mono{id}{downarrow}" => MonoDown(Set(Id("id")))
            case _ => throw new IllegalArgumentException(s"Unknown hyper type: $s")
        }
    }

    def programPreamble(params: Seq[(String, Seq[HyperType])]) : String = {
        val paramString = params.map { case (name, types) => s"$name: Int[${types.map(_.toString).mkString(",")}]" }.mkString(",")
        "method test(" + paramString + ") returns (o : Int) {"
    }

    def programEpilogue : String = {
        """
        }
        """
    }
}

trait RuleSoundnessTest {
    val testFolder =  "/home/ramon/ETH/SP/hypra_fork/src/test/ruleTests"
}


object HyperTypeExpressionAddition  extends RuleSoundnessTest {
    val testName = "HyperTypeExpressionAddition"
    val latexSource: String ="""
        low & low & low 
        pos & pos & pos 
        pos & zero & pos 
        zero & pos & pos 
        zero & negative & negative 
        negative & zero & negative 
        negative & negative & negative 
        pos, absGtOne & negative, absLtOne & pos 
        negative, absLtOne & pos, absGtOne & pos 
        negative, absGtOne & pos, absLtOne & negative 
        pos, absLtOne & negative, absGtOne & negative 
        mono{id}{uparrow} & low & mono{id}{uparrow} 
        low  & mono{id}{uparrow}& mono{id}{uparrow}  
        mono{id}{downarrow} & low & mono{id}{downarrow} 
        low  & mono{id}{downarrow}& mono{id}{downarrow} 
        absOne & zero & absOne 
        absGtOne & zero & absGtOne 
        absLtOne & zero & absLtOne 
        zero & absOne   & absOne 
        zero & absGtOne & absGtOne 
        zero & absLtOne & absLtOne 
        absGtOne, pos & pos & absGtOne
        absGtOne, pos & zero & absGtOne
        pos  & absGtOne, pos & absGtOne
        zero & absGtOne, pos & absGtOne
        absGtOne, negative & negative & absGtOne
        absGtOne, negative & zero & absGtOne
        negative  & absGtOne, negative & absGtOne
        zero & absGtOne, negative & absGtOne
        absLtOne, pos & absLtOne, negative & absLtOne 
        absLtOne, negative & absLtOne, pos & absLtOne 
        """


    def generateTestPrograms : Unit = {
        val tests = latexSource.split("\n").filter(_.nonEmpty).map(_.trim).filter(_.nonEmpty)
        
        tests.zipWithIndex.foreach { case (test, index) =>
            generateTestProgram(test, index)
        }
    }


    def generateTestProgram(test: String, testIndex: Int) : Unit = {
        val variables = test.split("&").map(_.trim).map(_.split(",")).map( variable => variable.map( typ => TestGeneratorHelper.stringToHyperType(typ.trim) ).toSeq ).toSeq

        // everything except the last variable is a precondition

        // add to every precondition a unique variable name
        var preconditions = variables.init.zipWithIndex.map { case (types, index) =>
            val varName = s"v$index"
            (varName, types)
        }

        preconditions = preconditions :+ ("id", Seq.empty[HyperType]) // add an empty variable for the id

        val postcondition = ("o", variables.last)

        val preconditionProgram = preconditions.map { case (varName, types) =>
            types.map { typ =>
                s"unfold ($typ) $varName"
            }.mkString("\n")
        }.mkString("\n")

        val postCondition = postcondition._2.map { typ =>
            s"fold ($typ) ${postcondition._1}"
        }.mkString("\n")

        val preamble = TestGeneratorHelper.programPreamble(preconditions)
        val epilogue = TestGeneratorHelper.programEpilogue

        val body = s"o := ${preconditions.head._1} + ${preconditions(1)._1}"

        val program = s"$preamble\n$preconditionProgram\n$body\n$postCondition\n$epilogue"

        // save the program to a file

        val path = s"${testFolder}/${testName}_${testIndex}.hhl"

        val writer = new PrintWriter(path)
        try {
            writer.write(program)
        } finally {
            writer.close()
        }
    }
}



object RuleSoundnessTests {


    def runTest(testPath: String) : Unit = {
        SymbolChecker.reset()
        TypeChecker.reset()
        Generator.reset()
        val programSource = scala.io.Source.fromFile(testPath)
        val program = programSource.mkString
        programSource.close()


        // [DOC] parse program
        val res = fastparse.parse(program, Parser.program(_))

        if (res.isSuccess) {

            var parsedProgram = res.get.value
            HyperTypeChecker.typeCheckProg(parsedProgram)
            parsedProgram = HyperTranslate.translateProgram(parsedProgram)
            SymbolChecker.checkSymbolsProg(parsedProgram)
            println("symbol check done")
            val viperProgram = Generator.generate(parsedProgram, program)
            val consistencyErrors = viperProgram.checkTransitively
            if (consistencyErrors.isEmpty) {
                val result = ViperRunner.runSiliconAndCarbon(viperProgram)
                result match {
                    case Success =>
                        println(s"Test at $testPath passed successfully.")
                    case Failure(errors) =>
                        throw new RuntimeException(s"Test at $testPath failed with errors: ${errors.mkString(", ")}")
                }
            } else {
                throw new RuntimeException(s"Test at $testPath failed with errors: ${consistencyErrors.mkString(", ")}")
            }
        } else {
            throw new RuntimeException(s"Parsing failed for test at $testPath")
        }
    }

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
        testExpressionRule(AdditionDerivationRule())
        return
        HyperTypeExpressionAddition.generateTestPrograms

        // list all files in the test folder
        val testPaths = new java.io.File(HyperTypeExpressionAddition.testFolder).listFiles
          .filter(_.getName.startsWith(HyperTypeExpressionAddition.testName))
          .map(_.getPath).toSeq


        var successTests = Seq.empty[String]
        var failureTests = Seq.empty[String]

        testPaths.foreach { testPath =>
            try {
                runTest(testPath)
                successTests :+= testPath
            } catch {
                case e: Exception =>
                    println(s"Test at $testPath failed with exception: ${e.getMessage}")
                    failureTests :+= testPath
            }        
        }

        if (successTests.nonEmpty) {
            println("Successful tests:")
            successTests.foreach(println)
        }
        if (failureTests.nonEmpty) {
            println("Failed tests:")
            failureTests.foreach(println)
        }
        println(s"Tests completed. Success: ${successTests.size}, Failures: ${failureTests.size}")
    }
}