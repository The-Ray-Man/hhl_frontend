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
            TypeChecker.typeCheckProg(parsedProgram)
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


    def main(args: Array[String]) : Unit = {
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