package viper.HHLVerifier.typing.dsl

import fastparse._
import fastparse.NoWhitespace._
import viper.HHLVerifier.typing.dsl.{Specification, DerivationRule}
import viper.HHLVerifier.typing.dsl.Mappings._

object Parser {
  def specification[$: P]: P[Specification]   = P(Start ~ derivationRule.rep(1, sep = "\n") ~ End).map(rules => Specification(rules))
  def derivationRule[$: P]: P[DerivationRule] = P(expressionDerivationRule)

  def expressionDerivationRule[$: P]: P[ExpressionDerivationRule] = P(
    functionPreamble ~ spaces ~
      functionArguments ~ spaces ~
      "=" ~ spaces ~
      expressionRules
  ).map { case (op, inputs, rules) =>
    ExpressionDerivationRule(op, inputs, rules)
  }

  def expressionRules[$: P]: P[Seq[Rule]] = P("[" ~ newlines ~ spaces ~ expressionRule.rep(sep = newlines) ~ spaces ~ newlines ~ "]")

  def functionPreamble[$: P]: P[String]                                   = P("|" ~ spaces ~ operator ~ spaces ~ "|")
  def functionArguments[$: P]: P[Seq[(HyperCollection, DeltaCollection)]] = P("(" ~~ spaces ~ gamma ~~ spaces ~ "," ~~ spaces ~ delta ~~ spaces ~ ("," ~~ spaces ~ combinationFunctionInputPair).rep ~ spaces ~ ")")
    .map { case (_, _, combinationPairs) => combinationPairs }

  def combinationFunctionInputPair[$: P]: P[(HyperCollection, DeltaCollection)] = P(spaces ~~ hyperCollection ~~ spaces ~ "," ~~ spaces ~ deltaCollection ~~ spaces)
  def expressionRule[$: P]: P[Rule]                                             = P(condition.rep ~~ spaces ~ "=>" ~~ spaces ~ conclusion.rep(1)).map { case (conds, conclusion) =>
    Rule(conds, conclusion) // Placeholder, replace with actual rule creation logic
  }

  def hyperCollection[$: P]: P[HyperCollection]             = P("H" ~ CharIn("0-9").rep(1).!.map(_.toInt)).map(id => HyperCollection(id))
  def deltaCollection[$: P]: P[DeltaCollection]             = P("D" ~ CharIn("0-9").rep(1).!.map(_.toInt)).map(id => DeltaCollection(id))
  def hyperCollectionResult[$: P]: P[HyperCollectionResult] = P("H").map(_ => HyperCollectionResult())
  def deltaCollectionResult[$: P]: P[DeltaCollectionResult] = P("D").map(_ => DeltaCollectionResult())
  def gamma[$: P]: P[Gamma]                                 = P("Gamma").map(_ => Gamma())
  def delta[$: P]: P[Delta]                                 = P("Delta").map(_ => Delta())

  def mapping[$: P]: P[Mapping] = P(deltaCollection | gamma | delta | deltaCollectionResult)

  def mappingAccess[$: P]: P[MappingAccess] = P(mapping ~ "(" ~ CharIn("a-zA-Z").rep(1).! ~ ")").map(x => mapMappingAccess(x._1, x._2))

  def set[$: P]: P[Set] = P(hyperCollection | mappingAccess | hyperCollectionResult)

  def condition[$: P]: P[Condition] = P(inSet)

  def inSet[$: P]: P[InSet]     = P(element ~~ spaces ~ "in" ~~ spaces ~ set).map { case (elem, set) => InSet(elem, set) }
  def element[$: P]: P[Element] = P(CharIn("a-zA-Z").rep(1).!.map(Element))

  def conclusion[$: P]: P[Conclusion] = P(addToSet)
  def addToSet[$: P]: P[AddToSet]     = P(element ~~ spaces ~ "addTo" ~~ spaces ~ set).map { case (elem, set) => AddToSet(elem, set) }
  def setEquals[$: P]: P[SetEquals]   = P(set ~~ spaces ~ "=" ~~ spaces ~ set).map { case (set1, set2) => SetEquals(set1, set2) }

  def operator[$: P]: P[String] = P("+" | "-" | "*" | "/" | "var" | "n").!

  def spaces[$: P]: P[Unit]   = P(CharsWhileIn(" \t").rep)
  def newlines[$: P]: P[Unit] = P(CharsWhileIn("\n").rep)
}
