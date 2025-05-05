package viper.HHLVerifier.test

import viper.HHLVerifier.Main
import au.com.bytecode.opencsv.CSVWriter
import viper.HHLVerifier.management.ViperRunner
import viper.HHLVerifier.parsing.Parser

import java.io.{BufferedWriter, File, FileWriter}
import scala.jdk.CollectionConverters._
import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.generation.Generator





trait TestExpected {}
case class ParseSuccess() extends TestExpected
case class ParseFailure() extends TestExpected
case class Unknown() extends TestExpected

trait TypeResult {}
case class TypeCheckSuccess() extends TypeResult
case class TypeCheckFails(message : String) extends TypeResult


object ParseTest {
  var success: List[String] = List.empty
  var failed: List[String] = List.empty
  var failedLog: List[String] = List.empty
  var totalNum = 0

  def getDataForTestCase(testPath: String): String = {
    val programSource = scala.io.Source.fromFile(testPath)
    val program = programSource.mkString
    programSource.close()
    program
  }

  def runTests(tests: List[(File, TestExpected)]): Unit = {
    Generator.autoSelectRules = true
    for (f <- tests) {
      println(s"starting with ${f._1.getPath}")
      totalNum = totalNum + 1
      val program = getDataForTestCase(f._1.getPath)

      val parsed = fastparse.parse(program, Parser.program(_))
      if (parsed.isSuccess) {
        // println(f"Parsed ${f._1.getPath} successfully")
        // success = success :+ f._1.getPath
        val parse_result = try {
          HyperTypeChecker.typeCheckProg(parsed.get.value)
          TypeCheckSuccess()
        } catch {
          case e: Exception => {
            TypeCheckFails(e.getMessage())
          }
        }
        (parse_result, f._2) match {
          case (TypeCheckSuccess(), ParseSuccess()) => {
            success = success :+ f._1.getPath
          }
          case (TypeCheckFails(message), ParseFailure()) => {
            success = success :+ f._1.getPath
          }
          case (TypeCheckSuccess(), _) => {
            failed = failed :+ f._1.getPath
            failedLog = failedLog :+ f"Typecheck success but should have failed"
          }
          case (TypeCheckFails(message), _) => {
            failed = failed :+ f._1.getPath
            failedLog = failedLog :+ f"Typecheck failed with message: \n$message"
          }
        }
        // println(parsed.get.value.methods(0).body.toString())
      } else {
        println(f"Failed to parse ${f._1.getPath}")
        failed = failed :+ f._1.getPath
        failedLog = failedLog :+ f"Failed to parse ${f._1.getPath} with error: ${parsed.toString()}"
      }
      
    }
  }

  def main(args: Array[String]): Unit = {
    val pathOfParseTests = "src/test/types"
    val typeTests = getListOfFiles(pathOfParseTests, Unknown())
    println(typeTests)
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

  def getListOfFiles(dir: String, expectedTestresult : TestExpected): List[(File, TestExpected)] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val content = d.listFiles
      val files = content.filter((file) => file.isFile && !file.getName().startsWith("_")).map(f => (f, expectedTestresult)).toList
      val subDir = content.filter(_.isDirectory).toList
      files ++ subDir.flatMap(subD => getListOfFiles(subD.getPath, subD.getName() match {
        case "valid" => ParseSuccess()
        case "invalid" => ParseFailure()
        case _ => expectedTestresult
      }))
    } else {
      List[(File, TestExpected)]()
    }
  }
}


