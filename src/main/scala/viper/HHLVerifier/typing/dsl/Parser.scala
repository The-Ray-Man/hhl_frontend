package viper.HHLVerifier.typing.dsl

import fastparse._
import fastparse.JavaWhitespace._
import viper.HHLVerifier.typing.dsl.{Specification, DerivationRule}
import viper.HHLVerifier.typing.dsl.Mappings._
import viper.HHLVerifier.parsing.{Parser => HypraParser}
import viper.HHLVerifier.ast.Id
// import fastparse.MultiLineWhitespace

object Parser {
  def specification[$: P]: P[Specification] =
    P(Start ~ derivationRule.rep(sep = ";") ~ ws ~ End)
      .map(rules => Specification(rules))

  def derivationRule[$: P]: P[DerivationRule] = P(expressionDerivationRule)

  def expressionDerivationRule[$: P]: P[ExpressionDerivationRule] = P(
    functionPreamble ~ ws ~
      functionArguments ~ ws ~
      "=" ~ ws ~
      expressionRules
  ).map {
    case (op, inputs, rules) => {
      ExpressionDerivationRule(op, inputs, rules)
    }
  }

  def expressionRules[$: P]: P[Seq[Rule]] = P("[" ~ expressionRule.rep(sep = ",") ~ ws ~ "]")

  def functionPreamble[$: P]: P[String]                                   = P("|" ~ ws ~ operator ~ ws ~ "|")
  def functionArguments[$: P]: P[Seq[(HyperCollection, DeltaCollection)]] = P("(" ~ ws ~ gamma ~ ws ~ "," ~ ws ~ delta ~ ws ~ ("," ~ ws ~ combinationFunctionInputPair).rep ~ ws ~ ")")
    .map { case (_, _, combinationPairs) => combinationPairs }

  def combinationFunctionInputPair[$: P]: P[(HyperCollection, DeltaCollection)] = P(hyperCollection ~ ws ~ "," ~ ws ~ deltaCollection)
  def expressionRule[$: P]: P[Rule]                                             = P((ws ~ condition ~ ws).rep(sep = "&&") ~ ws ~ "=>" ~ (ws ~ conclusion ~ ws).rep(1, sep = "&&")).map { case (conds, conclusion) =>
    Rule(conds, conclusion) // Placeholder, replace with actual rule creation logic
  }

  def hyperCollection[$: P]: P[HyperCollection]             = P("H" ~ CharIn("0-9").rep(1).!.map(_.toInt)).map(id => HyperCollection(id))
  def deltaCollection[$: P]: P[DeltaCollection]             = P("D" ~ CharIn("0-9").rep(1).!.map(_.toInt)).map(id => DeltaCollection(id))
  def hyperCollectionResult[$: P]: P[HyperCollectionResult] = P("H").map(_ => HyperCollectionResult())
  def deltaCollectionResult[$: P]: P[DeltaCollectionResult] = P("D").map(_ => DeltaCollectionResult())
  def gamma[$: P]: P[Gamma]                                 = P("Gamma").map(_ => Gamma())
  def delta[$: P]: P[Delta]                                 = P("Delta").map(_ => Delta())

  def mapping[$: P]: P[Mapping] = P(deltaCollection | gamma | delta | deltaCollectionResult)

  def mappingAccess[$: P]: P[MappingAccess] = P(mapping ~ "(" ~ variable ~ ")").map(x => MappingAccess(x._1, x._2))

  def set[$: P]: P[Set] = P(hyperCollection | mappingAccess | hyperCollectionResult)

  def condition[$: P]: P[Condition] = P(inSet | arithCondition | boolCondition | negatedCondition)

  def arithCondition[$: P] : P[ArithCondition] = P(
    "n" ~ ws ~ comparator ~ ws ~ CharIn("0-9").rep(1).!.map(_.toInt)
  ).map { case (comp, value) => ArithCondition(comp, value) }

  def boolCondition[$: P]: P[BoolCondition] = P(
    "b".!
  ).map { _ => BoolCondition() }

  def negatedCondition[$: P]: P[Condition] = P(
    "!" ~ ws ~ "(" ~ condition ~ ")"
  ).map { case cond => NotOperator(cond) }

  def comparator[$: P]: P[String] = P(">" | "<" | ">=" | "<=" | "==" | "!=").!

  def inSet[$: P]: P[InSet]                     = P(element ~ ws ~ "in" ~ ws ~ set).map { case (elem, set) => InSet(elem, set) }
  def element[$: P]: P[Element]                 = P(variable | hyperType)
  def variable[$: P]: P[Id]                     = P(CharIn("a-z").rep(1).!.map(Id))
  def hyperType[$: P]: P[HyperType]             = P(hyperTypeWithSetArgs | hyperTypeWithListArgs | simpleHyperType)
  def simpleHyperType[$: P]: P[SimpleHyperType] = P(CharIn("A-Z").rep(1).!.map(SimpleHyperType))
  def hyperTypeWithSetArgs[$: P]                = P(simpleHyperType ~ "{" ~ element.rep(1, sep = ",") ~ "}").map { case (name, args) =>
    HyperTypeWithSetArgs(name, args.toSet)
  }
  def hyperTypeWithListArgs[$: P] = P(simpleHyperType ~ "(" ~ element.rep(1, sep = ",") ~ ")").map { case (name, args) =>
    HyperTypeWithListArgs(name, args.toSeq)
  }

  def conclusion[$: P]: P[Conclusion] = P(addToSet | setEquals)
  def addToSet[$: P]: P[AddToSet]     = P(element ~ ws ~ "addTo" ~ ws ~ set).map { case (elem, set) => AddToSet(elem, set) }
  def setEquals[$: P]: P[SetEquals]   = P(set ~ ws ~ "=" ~ ws ~ set).map { case (set1, set2) => SetEquals(set1, set2) }

  def operator[$: P]: P[String] = P(
    "negate" |
      "var" |
      "n" |
      "b" |
      "++" |
      "+" |
      "&&" |
      "/" |
      "==>" |
      "==" |
      ">=" |
      ">" |
      "in" |
      "!=" |
      "%" |
      "*" |
      "||" |
      "setminus" |
      "set" |
      "<=" |
      "<" |
      "-" |
      "union" |
      "!" |
      "methodCall" |
      "lookup" |
      "length"
  ).!

  def ws[$: P]: P[Unit]               = P(CharsWhileIn(" \r\n\t").rep)
  def newlineSeparator[$: P]: P[Unit] = P(CharsWhileIn(" \t").? ~ ("\r\n" | "\n").rep(1))

}
