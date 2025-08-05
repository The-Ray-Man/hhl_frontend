package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.dsl.MappingAccess

object DSLTests {

  def main(args: Array[String]): Unit = {

    val parseSet = fastparse.parse("Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.set(_))
    println("1", parseSet)

    val parseCondition = fastparse.parse("pos in Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.condition(_))
    println("2", parseCondition)

    val parseConclusion = fastparse.parse("pos addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.conclusion(_))
    println("3", parseConclusion)

    val parseRule = fastparse.parse("pos in Gamma(var) => pos addTo Gamma(var)", viper.HHLVerifier.typing.dsl.Parser.expressionRule(_))
    println("4", parseRule)

    val functionPreamble = fastparse.parse("|+|", viper.HHLVerifier.typing.dsl.Parser.functionPreamble(_))
    println("5", functionPreamble)

    val functionArguments = fastparse.parse("(Gamma, Delta)", viper.HHLVerifier.typing.dsl.Parser.functionArguments(_))
    println("6", functionArguments)
    val functionArguments2 = fastparse.parse("(Gamma, Delta, H1, D1)", viper.HHLVerifier.typing.dsl.Parser.functionArguments(_))
    println("7", functionArguments2)

    val hyperCollection = fastparse.parse("H1", viper.HHLVerifier.typing.dsl.Parser.hyperCollection(_))
    println("8", hyperCollection)

    val deltaCollection = fastparse.parse("D1", viper.HHLVerifier.typing.dsl.Parser.deltaCollection(_))
    println("9", deltaCollection)

    val parseDerivationRule = fastparse.parse("|+|(Gamma, Delta) = []", viper.HHLVerifier.typing.dsl.Parser.expressionDerivationRule(_))
    println("10", parseDerivationRule)

    val testRules = fastparse.parse("[]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    println("11", testRules)

    val testRulesNoneEmpty = fastparse.parse("[pos in Gamma(var) => pos addTo Gamma(var)]", viper.HHLVerifier.typing.dsl.Parser.expressionRules(_))
    println("12", testRulesNoneEmpty)

    val combinationFunctionInputPair = fastparse.parse("H1, D1", viper.HHLVerifier.typing.dsl.Parser.combinationFunctionInputPair(_))
    println("13", combinationFunctionInputPair)

    val filePath    = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/testSimple.type"
    val fileContent = scala.io.Source.fromFile(filePath).getLines().mkString("\n")
    println(fileContent)
    val res = fastparse.parse(fileContent, viper.HHLVerifier.typing.dsl.Parser.specification(_))

    println(res)

  }
}
