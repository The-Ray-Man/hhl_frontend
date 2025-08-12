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
import viper.HHLVerifier.typing.rules.TypeSystem

object DerivationTests {

  def runTest(typeSystem: TypeSystem, expression: Expr, gamma: HyperMapping, delta: DeltaMapping, expectedHT : Option[HyperTypeCollection], expectedHTDT: Option[DeltaCollection]) = {
    val result = typeSystem.deriveExpression(gamma, delta, expression, Map())
    if (expectedHT.isDefined) {
      assert(result.hyperTypeCollection == expectedHT.get, s"Expected hyper type collection ${expectedHT.get} but got ${result.hyperTypeCollection}")
    }
    if (expectedHTDT.isDefined) {
      assert(result.deltaCollection == expectedHTDT.get, s"Expected delta collection ${expectedHTDT.get} but got ${result.deltaCollection}")
    }
  }

  def valueTests(): Unit = {
    val valuePath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    val typeSystem = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(valuePath))

    val mapping = HyperMapping(Map())
    val deltaMapping = DeltaMapping(Map())

    val pos =Some(HyperTypeCollection(Set(SimpleHyperType("POS"))))
    val zero = Some(HyperTypeCollection(Set(SimpleHyperType("ZERO"))))
    val neg = Some(HyperTypeCollection(Set(SimpleHyperType("NEG"))))
    val trueLit = Some(HyperTypeCollection(Set(SimpleHyperType("TRUE"))))
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


  def infFlowTests() : Unit = {
    val infFlowPath       = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    val typeSystem = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(infFlowPath))


    val mappingXLow = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    val mappingXLowYLow = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW")))), ("y", HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    val mappingXLowYHigh = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW")))), ("y", HyperTypeCollection(Set()))))

    val expression1 = BinaryExpr(Id("x"), "+", Num(2))
    val expression2 = BinaryExpr(Id("x"), "/", Id("y"))
    val expression3 = BinaryExpr(Id("x"), "-", Id("y"))

    val expectedHTLow = Some(HyperTypeCollection(Set(SimpleHyperType("LOW"))))
    val expectedHTHigh = Some(HyperTypeCollection(Set()))

    runTest(typeSystem, expression1, mappingXLow, DeltaMapping(Map()), expectedHTLow, None)
    runTest(typeSystem, expression2, mappingXLowYLow, DeltaMapping(Map()), expectedHTLow, None)
    runTest(typeSystem, expression3, mappingXLowYHigh, DeltaMapping(Map()), expectedHTHigh, None)

  }

  def main(args: Array[String]): Unit = {
    infFlowTests()
    valueTests()
    println("all tests passed")
  }
}
