package viper.HHLVerifier.test

import viper.HHLVerifier.Main
import au.com.bytecode.opencsv.CSVWriter
import viper.HHLVerifier.management.ViperRunner
import viper.HHLVerifier.parsing.Parser

import java.io.{BufferedWriter, File, FileWriter}
import scala.jdk.CollectionConverters._
import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.generation.Generator
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.symbols.SymbolChecker
import viper.HHLVerifier.typing.TypeChecker
import viper.silicon.rules.evaluator.eval
import viper.silver.verifier.{Failure => ResFailure, Success => ResSuccess}
import viper.HHLVerifier.typing.HyperTranslate
import viper.HHLVerifier.typing.dsl.TypeSystem

trait VerificationResult {}
case class VerificationSuccess() extends VerificationResult
case class VerificationFailure() extends VerificationResult
case class UnknownVerification() extends VerificationResult

trait TestResult {}
case class TypeCheckSuccess(verifyResult: VerificationResult) extends TestResult
case class TypeCheckFailure(message: Option[String])          extends TestResult
case class Unknown()                                          extends TestResult

object HyperTest {
  var success: List[String]   = List.empty
  var failed: List[String]    = List.empty
  var failedLog: List[String] = List.empty
  var totalNum                = 0

  def getDataForTestCase(testPath: String): String = {
    val programSource = scala.io.Source.fromFile(testPath)
    val program       = programSource.mkString
    programSource.close()
    program
  }

  def evaluateResult(result: TestResult, test: (File, TestResult)) = {
    (result, test._2) match {
      case (TypeCheckSuccess(VerificationSuccess()), TypeCheckSuccess(VerificationSuccess())) | (TypeCheckSuccess(VerificationFailure()), TypeCheckSuccess(VerificationFailure())) | (TypeCheckSuccess(_), TypeCheckSuccess(UnknownVerification())) | (TypeCheckFailure(_), TypeCheckFailure(_)) => {
        success = success :+ test._1.getPath
      }
      case (TypeCheckSuccess(VerificationSuccess()), TypeCheckSuccess(VerificationFailure())) => {
        failed = failed :+ test._1.getPath
        failedLog = failedLog :+ f"Verification success but should have failed"
      }
      case (TypeCheckSuccess(VerificationFailure()), TypeCheckSuccess(VerificationSuccess())) => {
        failed = failed :+ test._1.getPath
        failedLog = failedLog :+ f"Verification failed but should have succeeded"
      }
      case (TypeCheckFailure(msg), TypeCheckSuccess(_)) => {
        failed = failed :+ test._1.getPath
        failedLog = failedLog :+ f"Typecheck failed but should have succeeded : $msg"
      }
      case (TypeCheckSuccess(_), TypeCheckFailure(_)) => {
        failed = failed :+ test._1.getPath
        failedLog = failedLog :+ f"Typecheck succeed but should have failed"
      }
      case (_, _) => {
        failed = failed :+ test._1.getPath
        failedLog = failedLog :+ f"Unexpected result for ${test._1.getPath}: $result"
      }

    }
  }

  def verificationCheck(program: HHLProgram, test: (File, TestResult)) = {
    Generator.verifierOption = 2
    Generator.autoSelectRules = true
    val translatedProgram = HyperTranslate.translateProgram(program)
    SymbolChecker.checkSymbolsProg(translatedProgram)
    TypeChecker.typeCheckProg(translatedProgram)
    val viperProgram = Generator.generate(translatedProgram, test._1.getPath)
    SymbolChecker.reset()
    TypeChecker.reset()
    Generator.reset()
    val consistencyErrors = viperProgram.checkTransitively
    // We check whether the program is well-defined (i.e., has no consistency errors such as ill-typed expressions)
    if (consistencyErrors.nonEmpty) {
      failed = failed :+ test._1.getPath
      failedLog = failedLog :+ (f"A consistency error occured: ${consistencyErrors.map(err => err.readableMessage).mkString("\n")}")
    } else {
      val result = ViperRunner.runSiliconAndCarbon(viperProgram)
      result match {
        case ResSuccess =>
          evaluateResult(TypeCheckSuccess(VerificationSuccess()), test)
        case ResFailure(err) =>
          evaluateResult(TypeCheckSuccess(VerificationFailure()), test)
      }
    }
  }

  def hyperTypeCheck(program: HHLProgram, test: (File, TestResult)): Unit = {
    val system = TypeSystem()
    try {
      HyperTypeChecker.typeCheckProg(system, program)
    } catch {
      case e: Exception => {
        val error = TypeCheckFailure(Some(e.getMessage()))
        evaluateResult(error, test)
        return
      }
    }
    verificationCheck(program, test)
  }

  def runTests(tests: List[(File, TestResult)]): Unit = {
    for (f <- tests) {
      SymbolChecker.reset()
      TypeChecker.reset()
      Generator.reset()
      Generator.autoSelectRules = true
      println(s"starting with ${f._1.getPath}")
      totalNum = totalNum + 1
      val program = getDataForTestCase(f._1.getPath)

      val parsed = fastparse.parse(program, Parser.program(_))
      if (parsed.isSuccess) {
        var parsedProgram = parsed.get.value
        hyperTypeCheck(parsedProgram, f)
      } else {
        println(f"Failed to parse ${f._1.getPath}")
        failed = failed :+ f._1.getPath
        failedLog = failedLog :+ f"Failed to parse ${f._1.getPath} with error: ${parsed.toString()}"
      }

    }
  }

  def main(): Unit = {
    val pathOfHyperTests = "src/test/hyperTypes"
    val typeTests        = getAllTestFiles(pathOfHyperTests)

    for (test <- typeTests) {
      println(f"Test: ${test._1.getPath} with expected result: ${test._2}")
    }
    // return

    runTests(typeTests)
    println("Success: " + success.length)
    println("Failed: " + failed.length)
    println("Total: " + totalNum)

    println("=======ERRORS=======")
    // zip over failed and failedLog
    for (i <- failed.indices) {
      println(f"Failed: ${failed(i)}")
      println(f"Log: ${failedLog(i)}")
    }
  }

  def getAllTestFiles(dir: String): List[(File, TestResult)] = {
    println(f"Reading test files from $dir")
    val invalidHyperTypes      = getListOfFiles(dir + "/invalid", TypeCheckFailure(None))
    val validHyperTypeUnknwon  = getListOfFiles(dir + "/valid", TypeCheckSuccess(UnknownVerification()))
    val validHyperTypesCorrect = getListOfFiles(dir + "/valid/correct", TypeCheckSuccess(VerificationSuccess()))
    val validHyperTypesWrong   = getListOfFiles(dir + "/valid/wrong", TypeCheckSuccess(VerificationFailure()))
    invalidHyperTypes ++ validHyperTypesCorrect ++ validHyperTypesWrong ++ validHyperTypeUnknwon
  }

  def getListOfFiles(dir: String, expectedTestresult: TestResult): List[(File, TestResult)] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val content = d.listFiles
      val files   = content.filter((file) => file.isFile && !file.getName().startsWith("_")).map(f => (f, expectedTestresult)).toList
      return files
    }
    List.empty[(File, TestResult)]
  }
}
