package viper.HHLVerifier.test

import viper.HHLVerifier.Main
import au.com.bytecode.opencsv.CSVWriter
import viper.HHLVerifier.management.ViperRunner
import viper.HHLVerifier.parsing.Parser

import java.io.{BufferedWriter, File, FileWriter}
import scala.jdk.CollectionConverters._

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

  def runTests(tests: List[File]): Unit = {
    for (f <- tests) {
      totalNum = totalNum + 1
      val program = getDataForTestCase(f.getPath)
      val parsed = fastparse.parse(program, Parser.program(_))
      if (parsed.isSuccess) {
        println(f"Parsed ${f.getPath} successfully")
        success = success :+ f.getPath
        // println(parsed.get.value.methods(0).body.toString())
      } else {
        println(f"Failed to parse ${f.getPath}")
        failed = failed :+ f.getPath
        failedLog = failedLog :+ f"Failed to parse ${f.getPath} with error: ${parsed.toString()}"
      }
      
    }
  }

  def main(args: Array[String]): Unit = {
    val pathOfParseTests = "src/test/types"
    val typeTests = getListOfFiles(pathOfParseTests)
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

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val content = d.listFiles
      val files = content.filter(_.isFile).toList
      val subDir = content.filter(_.isDirectory).toList
      files ++ subDir.flatMap(subD => getListOfFiles(subD.getPath))
    } else {
      List[File]()
    }
  }
}


