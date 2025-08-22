package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.dsl.SimpleHyperType
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.dsl.TypeSystem
import viper.HHLVerifier.ast.AssignStmt
import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.ast.IfElseStmt
import viper.HHLVerifier.ast.HHLProgram
import viper.HHLVerifier.parsing.Parser
import viper.HHLVerifier.typing.HyperTypeChecker
import viper.HHLVerifier.generation.Generator

object DerivationTests {

  def equal(shouldHyperMapping : HyperMapping, is: HyperMapping) : Boolean  = {
    val keys = (shouldHyperMapping.mapping.keySet ++ is.mapping.keySet)
    keys.forall(key => shouldHyperMapping.mapping.getOrElse(key, HyperTypeCollection(Set())) == is.mapping.getOrElse(key, HyperTypeCollection(Set())))

  }

  def runTest(typeSystem: TypeSystem, expression: Expr, gamma: HyperMapping, delta: DeltaMapping, expectedHT: Option[HyperTypeCollection], expectedHTDT: Option[DeltaCollection]) = {
    val result = typeSystem.deriveExpression(gamma, delta, expression, Map())
    println(s"$expression |- {${result.hyperTypeCollection}} {${result.deltaCollection}}")

    if (expectedHT.isDefined) {
      assert(result.hyperTypeCollection == expectedHT.get, s"Expected hyper type collection ${expectedHT.get} but got ${result.hyperTypeCollection}")
    }
    if (expectedHTDT.isDefined) {
      assert(result.deltaCollection == expectedHTDT.get, s"Expected delta collection ${expectedHTDT.get} but got ${result.deltaCollection}")
    }
  }

  def runTest(typeSystem: TypeSystem, stmt: Stmt, gamma: HyperMapping, delta: DeltaMapping, expectedM: Option[HyperMapping], expectedDM: Option[DeltaMapping]) = {
    val result = typeSystem.deriveStatement(gamma, delta, stmt, HyperTypeCollection(Set()))

    println(s"$stmt |- {${result.hyperTypeMapping}} {${result.deltaMapping}}")
    if (expectedM.isDefined && !equal(expectedM.get, result.hyperTypeMapping)) {
      throw new Exception(s"Expected hyper mapping ${expectedM.get} but got ${result.hyperTypeMapping}")
    }
    if (expectedDM.isDefined) {
      assert(result.deltaMapping == expectedDM.get, s"Expected delta mapping ${expectedDM.get} but got ${result.deltaMapping}")
    }
  }

  def runTest(typeSystem: TypeSystem, program: HHLProgram, shouldFail: Boolean): Unit = {
    Generator.autoSelectRules = true
    try {
      HyperTypeChecker.typeCheckProg(typeSystem, program)
      if (shouldFail) {
        throw new Exception(s"Program $program should have failed to type check but passed")
      }
    } catch {
      case e: Exception => {
        if (!shouldFail) {
          throw new Exception(s"Program failed to type check: ${e.getMessage} \n${e.getStackTrace.mkString("\n")}")
        }
      }
    }
  }

  def programFromFile(filePath: String): HHLProgram = {
    val programSource = scala.io.Source.fromFile(filePath)
    val program       = programSource.mkString
    programSource.close()
    val res = fastparse.parse(program, Parser.program(_))
    if (res.isSuccess) {
      res.get.value
    } else {
      throw new Exception(s"Could not parse program from file $filePath")
    }

  }

  def valueTests(): Unit = {
    val valuePath  = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    val stmtPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val typeSystem = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(valuePath, stmtPath))

    val mapping      = HyperMapping(Map())
    val deltaMapping = DeltaMapping(Map())

    val pos      = Some(HyperTypeCollection(Set(SimpleHyperType("POS"))))
    val zero     = Some(HyperTypeCollection(Set(SimpleHyperType("ZERO"))))
    val neg      = Some(HyperTypeCollection(Set(SimpleHyperType("NEG"))))
    val trueLit  = Some(HyperTypeCollection(Set(SimpleHyperType("TRUE"))))
    val falseLit = Some(HyperTypeCollection(Set(SimpleHyperType("FALSE"))))

    val expression1 = Num(3)
    runTest(typeSystem, expression1, mapping, deltaMapping, pos, None)

    val expression2 = Num(0)
    runTest(typeSystem, expression2, mapping, deltaMapping, zero, None)

    val expression3 = Num(-1)
    runTest(typeSystem, expression3, mapping, deltaMapping, neg, None)

    val expression4 = BoolLit(true)
    runTest(typeSystem, expression4, mapping, deltaMapping, trueLit, None)

    val expression5 = BoolLit(false)
    runTest(typeSystem, expression5, mapping, deltaMapping, falseLit, None)

    val expression6 = BinaryExpr(Num(3), "+", Num(2))
    runTest(typeSystem, expression6, mapping, deltaMapping, pos, None)

    val expression7 = BinaryExpr(Num(3), ">", Num(-2))
    runTest(typeSystem, expression7, mapping, deltaMapping, trueLit, None)

  }

  def infFlowTests(): Unit = {
    val infFlowPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    val stmtPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val typeSystem  = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(infFlowPath, stmtPath))

    val mappingXLow      = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    val mappingXLowYLow  = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW")))), ("y", HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    val mappingXLowYHigh = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW")))), ("y", HyperTypeCollection(Set()))))

    val expression1 = BinaryExpr(Id("x"), "+", Num(2))
    val expression2 = BinaryExpr(Id("x"), "/", Id("y"))
    val expression3 = BinaryExpr(Id("x"), "-", Id("y"))

    val expectedHTLow  = Some(HyperTypeCollection(Set(SimpleHyperType("LOW"))))
    val expectedHTHigh = Some(HyperTypeCollection(Set()))

    runTest(typeSystem, expression1, mappingXLow, DeltaMapping(Map()), expectedHTLow, None)
    runTest(typeSystem, expression2, mappingXLowYLow, DeltaMapping(Map()), expectedHTLow, None)
    runTest(typeSystem, expression3, mappingXLowYHigh, DeltaMapping(Map()), expectedHTHigh, None)

  }

  def valueInfFlowTests(): Unit = {
    val infFlowPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    val valuePath   = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    val stmtPath    = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val typeSystem  = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(infFlowPath, valuePath, stmtPath))

    val mapping     = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"), SimpleHyperType("POS"))))))
    val expression1 = BinaryExpr(Id("x"), "+", Num(2))
    val expression2 = BinaryExpr(Id("x"), ">=", Num(0))

    val expected1 = Some(HyperTypeCollection(Set(SimpleHyperType("POS"), SimpleHyperType("LOW"))))
    val expected2 = Some(HyperTypeCollection(Set(SimpleHyperType("TRUE"), SimpleHyperType("LOW"))))
    runTest(typeSystem, expression1, mapping, DeltaMapping(Map()), expected1, None)
    runTest(typeSystem, expression2, mapping, DeltaMapping(Map()), expected2, None)
  }

  def deltaTests(): Unit = {
    val deltaPath   = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/deltaOnValue.type"
    val infFlowPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    val valuePath   = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    val stmtPath    = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val typeSystem  = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(deltaPath, infFlowPath, valuePath, stmtPath))

    val gamma1      = HyperMapping(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("LOW")))))
    val expression1 = Id("x")
    val expectedHT  = Some(HyperTypeCollection(Set(SimpleHyperType("LOW"))))
    val expectedDT  = Some(DeltaCollection(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("ZERO"), SimpleHyperType("LOW"))))))
    runTest(typeSystem, expression1, gamma1, DeltaMapping(Map()), expectedHT, expectedDT)

    val expression2 = BinaryExpr(Id("x"), "+", Num(2))
    val expectedHT2 = Some(HyperTypeCollection(Set(SimpleHyperType("LOW"))))
    val expectedDT2 = Some(DeltaCollection(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("POS"), SimpleHyperType("LOW"))))))
    runTest(typeSystem, expression2, gamma1, DeltaMapping(Map()), expectedHT2, expectedDT2)
  }

  def statementTests(): Unit = {
    Generator.autoSelectRules = true
    val infFlowPath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    val stmtPath    = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/stmt.type"
    val typeSystem  = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(infFlowPath, stmtPath))

    val gamma1     = HyperMapping(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("LOW")))))
    val statement  = AssignStmt(Id("y"), BinaryExpr(Id("x"), "+", Num(2)))
    val expectedHM = Some(HyperMapping(Map("y" -> HyperTypeCollection(Set(SimpleHyperType("LOW"))), "x" -> HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    runTest(typeSystem, statement, gamma1, DeltaMapping(Map()), expectedHM, None)

    val statement2 = CompositeStmt(
      Seq(
        AssignStmt(Id("x"), Num(3)),
        AssignStmt(Id("y"), BinaryExpr(Id("x"), "+", Num(2)))
      )
    )
    runTest(
      typeSystem,
      statement2,
      HyperMapping(Map()),
      DeltaMapping(Map()),
      Some(HyperMapping(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("LOW"))), "y" -> HyperTypeCollection(Set(SimpleHyperType("LOW")))))),
      None
    )

    val statement3 = IfElseStmt(
      BoolLit(true),
      CompositeStmt(Seq(AssignStmt(Id("x"), Num(3)))),
      CompositeStmt(Seq(AssignStmt(Id("x"), Num(4))))
    )

    runTest(
      typeSystem,
      statement3,
      HyperMapping(Map()),
      DeltaMapping(Map()),
      Some(HyperMapping(Map("x" -> HyperTypeCollection(Set(SimpleHyperType("LOW")))))),
      None
    )

    val statement4 = IfElseStmt(
      BinaryExpr(Id("x"), ">", Num(0)),
      CompositeStmt(Seq(AssignStmt(Id("y"), Num(3)))),
      CompositeStmt(Seq(AssignStmt(Id("y"), Num(4))))
    )

    runTest(
      typeSystem,
      statement4,
      HyperMapping(Map("x" -> HyperTypeCollection(Set()))),
      DeltaMapping(Map()),
      Some(HyperMapping(Map())),
      None
    )

    val program1 = programFromFile("/home/ramon/ETH/SP/hypra_fork/src/test/dslTestPrograms/infFlow1.hhl")
    runTest(typeSystem, program1, false)

    val program2 = programFromFile("/home/ramon/ETH/SP/hypra_fork/src/test/dslTestPrograms/infFlow2.hhl")
    runTest(typeSystem, program2, false)

    val valuePath   = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    val typeSystem2 = viper.HHLVerifier.typing.dsl.TypeSystem.loadTypeSystem(Seq(valuePath, stmtPath))

    val program3 = programFromFile("/home/ramon/ETH/SP/hypra_fork/src/test/dslTestPrograms/value1.hhl")
    runTest(typeSystem2, program3, false)
    val program4 = programFromFile("/home/ramon/ETH/SP/hypra_fork/src/test/dslTestPrograms/value2.hhl")
    runTest(typeSystem2, program4, false)

    val program5 = programFromFile("/home/ramon/ETH/SP/hypra_fork/src/test/dslTestPrograms/mono1.hhl")
    runTest(typeSystem2, program5, false)

  }

  def main(args: Array[String]): Unit = {
    deltaTests()
    infFlowTests()
    valueTests()
    valueInfFlowTests()
    statementTests()
    println("all tests passed")
  }
}
