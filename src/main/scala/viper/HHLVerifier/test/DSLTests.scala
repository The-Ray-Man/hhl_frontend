package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.dsl.MappingAccess
import viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem

object DSLTests {

  def result(testName: String, res: fastparse.Parsed[Any]): Unit = {
    if (res.isSuccess) {
      println(s"$testName: Success")
    } else {
      println(s"$testName: Failure - ${res.asInstanceOf[fastparse.Parsed.Failure].msg}")
    }
  }

  def parsingTests(): Unit = {
    val parseSet = fastparse.parse("Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("1", parseSet)

    val parseCondition = fastparse.parse("LOW in H0", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("2", parseCondition)

    val parseConclusion = fastparse.parse("POS addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    result("3", parseConclusion)

    val parseRule = fastparse.parse("POS in Gamma(var) => POS addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("4", parseRule)

    val functionPreamble = fastparse.parse("| + |", viper.HHLVerifier.typing.dsl.Parser.functionPreamble(_))
    result("5", functionPreamble)

    val functionArguments = fastparse.parse("(Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.functionArguments(_))
    result("6", functionArguments)
    val functionArguments2 = fastparse.parse("(Gamma, Delta, H1, D1)", viper.HHLVerifier.typing.dsl.Parser.functionArguments(_))
    result("7", functionArguments2)

    val hyperCollection = fastparse.parse("H1", viper.HHLVerifier.typing.dsl.Parser.hyperCollection(_))
    result("8", hyperCollection)

    val deltaCollection = fastparse.parse("D1", viper.HHLVerifier.typing.dsl.Parser.deltaCollection(_))
    result("9", deltaCollection)

    val parseDerivationRule = fastparse.parse("|+|(Gamma, Delta) = []", viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_))
    result("10", parseDerivationRule)

    val testRules = fastparse.parse("[]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("11", testRules)

    val testRulesNoneEmpty = fastparse.parse("[POS in Gamma(var) => POS addTo Gamma(var)]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("12", testRulesNoneEmpty)

    val testRulesNoneTwoElements = fastparse.parse("[POS in Gamma(var) => POS addTo Gamma(var), POS in Gamma(var) => POS addTo Gamma(var)]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("12", testRulesNoneTwoElements)

    val combinationFunctionInputPair = fastparse.parse("H1, D1", viper.HHLVerifier.typing.dsl.Parser.combinationFunctionInputPair(_))
    result("13", combinationFunctionInputPair)

    val content     = """|n|(Gamma, Delta) = [
    => LOW
]"""
    val parseSystem = fastparse.parse(content, viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("14", parseRule)

    val content2     = """|var|(Gamma, Delta) = [];
|var|(Gamma, Delta) = []
"""
    val parseSystem2 = fastparse.parse(content2, viper.HHLVerifier.typing.dsl.Parser.specification(_))
    result("15", parseSystem2)

    val content3     = "[LOW in H0 => LOW addTo H,LOW in H1 => LOW addTo H]"
    val parseSystem3 = fastparse.parse(content3, viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("16", parseSystem3)

    val res17 = fastparse.parse("LOW", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("17", res17)
    val res18 = fastparse.parse("LOW{a,b,c}", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("18", res18)
    val res19 = fastparse.parse("LOW[a,b,c]", viper.HHLVerifier.typing.dsl.Parser.hyperType(_))
    result("19", res19)
    val res20 = fastparse.parse("LOW in H0", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("20", res20)

    val res21 = fastparse.parse("LOW{a,b,c} in H0 ", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("21", res21)

    val res22 = fastparse.parse("LOW{a,b,c} in H0 => LOW{a,b,c} addTo H1", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("22", res22)

    val res23 = fastparse.parse("[LOW{a,b,c} in H0 => LOW{a,b,c} addTo H1]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("23", res23)

    val res24 = fastparse.parse("[LOW{a,b,c} in H0 => LOW{a,b,c} addTo H1,\nLOW in H0 => LOW{a,b,c} addTo H1]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("24", res24)

    val res25 = fastparse.parse(
      "|+|(Gamma, Delta) = [LOW{a,b,c} in H0 => LOW{a,b,c} addTo H1,LOW in H0 => LOW{a,b,c} addTo H1]",
      viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_)
    )
    result("25", res25)

    val res26 = fastparse.parse(
      "|+|(Gamma, Delta) = [LOW{a,b,c} in H0 => LOW{a,b,c} addTo H1,\nLOW in H0 => LOW{a,b,c} addTo H1]",
      viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_)
    )
    result("26", res26)

    val res27 = fastparse.parse("[LOW in H0 => LOW addTo H,LOW in H1 => LOW addTo H]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    result("27", res27)

    val res28 = fastparse.parse("H0", viper.HHLVerifier.typing.dsl.Parser.set(_))
    result("28", res28)
    val res29 = fastparse.parse("LOW", viper.HHLVerifier.typing.dsl.Parser.element(_))
    result("29", res29)

    val res30 = fastparse.parse("LOW in H0", viper.HHLVerifier.typing.dsl.Parser.inSet(_))
    result("30", res30)

    val res31 = fastparse.parse("LOW in H0", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    result("31", res31)

    val res32 = fastparse.parse("LOW in H0 && LOW in H1 => LOW addTo H", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    result("32", res32)

  }
  def loadingTypeSystemTest(filename: String): Unit = {
    println("testing:", filename)
    val path       = s"/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/$filename"
    val _ = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(path))
  }

  def main(args: Array[String]): Unit = {
    parsingTests()
    loadingTypeSystemTest("value.type")
    loadingTypeSystemTest("infFlow.type")

  }
}
