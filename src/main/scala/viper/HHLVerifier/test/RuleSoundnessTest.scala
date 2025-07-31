package viper.HHLVerifier.test

import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.typing.HyperTranslate
import viper.HHLVerifier.symbols.SymbolChecker
import viper.HHLVerifier.typing.TypeChecker
import viper.HHLVerifier.generation.Generator
import viper.HHLVerifier.management.ViperRunner
import viper.silver.verifier.Success
import viper.silver.verifier.Failure 
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.typing
import viper.HHLVerifier.typing.rules.expression.AdditionDerivationRule





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

    def testExpressionRule(rule: typing.rules.ExpressionDerivationRule) =  {
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


    def main(): Unit = {
        testExpressionRule(AdditionDerivationRule())
    }
}