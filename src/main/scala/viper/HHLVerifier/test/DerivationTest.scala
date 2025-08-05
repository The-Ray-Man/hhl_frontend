package viper.HHLVerifier.test

import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.rules.ExpressionDerivationRule
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.rules.ExpressionTypeSystem
import viper.HHLVerifier.typing.dsl.SimpleHyperType
import viper.HHLVerifier.ast.BoolLit

object DerivationTests {

  def main(args: Array[String]): Unit = {
    val system           = ExpressionTypeSystem()
    var expression: Expr = null
    expression = BinaryExpr(Num(1), "+", Num(2))
    var mapping = HyperMapping(Map())
    var res     = system.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = BinaryExpr(Id("x"), "+", Num(2))
    mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"), SimpleHyperType("ZERO"))))))

    res = system.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = Id("x")
    mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"), SimpleHyperType("ZERO"))))))

    res = system.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    val infFlowPath       = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/infFlow.type"
    var typeSystem = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(infFlowPath))
    expression = BinaryExpr(Id("x"), "+", Num(2))
    mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"))))))
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")


    val valuePath = "/home/ramon/ETH/SP/hypra_fork/src/main/scala/viper/HHLVerifier/typing/dsl/rules/value.type"
    typeSystem = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(valuePath))
    expression = Num(3)
    mapping = HyperMapping(Map())
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = Num(0)
    mapping = HyperMapping(Map())
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = Num(-1)
    mapping = HyperMapping(Map())
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")


    expression = BoolLit(true)
    mapping = HyperMapping(Map())
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = BoolLit(false)
    mapping = HyperMapping(Map())
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    expression = fastparse.parse("(x + 3)*0 > z", viper.HHLVerifier.parsing.Parser.expr(_)) match {
      case fastparse.Parsed.Success(value, _) => value
      case failure: fastparse.Parsed.Failure  => throw new Exception(s"Failed to parse expression: ${failure.msg}")
    }
    mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("POS")))), ("z", HyperTypeCollection(Set(SimpleHyperType("POS"))))))
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")

    typeSystem = viper.HHLVerifier.typing.rules.TypeSystem.loadTypeSystem(Seq(valuePath, infFlowPath))
    expression = BinaryExpr(Id("x"), "+", Num(2))
    mapping = HyperMapping(Map(("x", HyperTypeCollection(Set(SimpleHyperType("LOW"), SimpleHyperType("ZERO"))))))
    res = typeSystem.ExpressionTypeSystem.derive(expression, mapping)
    println(s"$expression :: {${res._1}}, {${res._2}}")
    

  }
}
