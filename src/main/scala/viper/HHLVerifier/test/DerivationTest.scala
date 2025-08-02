package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule



object DerivationTests {

    def main(args: Array[String]): Unit = {


        val expression1 = BinaryExpr(Num(1), "+", Num(2))
        val mapping = HyperMapping(Map())

        val result = ExpressionDerivationRule.derive(expression1, mapping)
        println(s"Result of derivation: $result")
    }
}