package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.{Low, Zero}
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.rules.ExpressionSystem



object DerivationTests {

    def main(args: Array[String]): Unit = {
        val system = ExpressionSystem()
        var expression : Expr = null
        expression = BinaryExpr(Num(1), "+", Num(2))
        var mapping = HyperMapping(Map())
        var res = system.derive(expression, mapping)
        println(s"$expression :: {${res._1}}, {${res._2}}")


        expression = BinaryExpr(Id("x"), "+", Num(2))
        mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(Low(), Zero())))))

        res = system.derive(expression, mapping)
        println(s"$expression :: {${res._1}}, {${res._2}}")

        expression = Id("x") 
        mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(Low(), Zero())))))

        res = system.derive(expression, mapping)
        println(s"$expression :: {${res._1}}, {${res._2}}")

    }
}