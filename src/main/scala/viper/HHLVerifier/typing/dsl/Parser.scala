package viper.HHLVerifier.typing.dsl

import fastparse._
import fastparse.JavaWhitespace._
import viper.HHLVerifier.typing.dsl.{Specification, DerivationRule}
import viper.HHLVerifier.parsing.{Parser => HypraParser}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.Stmt
// import fastparse.MultiLineWhitespace

object Parser {
  def specification[$: P]: P[Specification] =
    P(Start ~ hyperTypeDeclaration.rep() ~ ws ~ derivationRule.rep(sep = ";") ~ ws ~ End)
      .map { case (declarations, rules) =>
        Specification(declarations, rules.toSeq)
      }

  def derivationRule[$: P]: P[DerivationRule] = P(statementDerivationRule | expressionDerivationRule)

  def hyperTypeDeclaration[$: P]: P[HyperTypeDeclaration] = P(variable ~ ws ~ ":" ~ hyperType ~ ws ~ "<=>" ~ expression).map { case (variable, hyperType, expression) =>
    HyperTypeDeclaration(variable, hyperType, expression)
  }

  def expression[$: P]: P[Expr] = (HypraParser.expr | ("-".! ~/ variable)).map {
    case (_, varName: Id) => UnaryExpr("-", varName)
    case (expr: Expr)     => expr
  }

  def statement[$: P]: P[StmtPattern] = P((variable ~ ":=" ~ variable).map { case (varName, expr) => AssignStmt(varName, expr) } | (variable ~ ";" ~ variable).map { case (firstStmt, secondStmt) => CompStmt(firstStmt, secondStmt) } | ("if" ~ ws ~ variable ~ ws ~ "then" ~ ws ~ variable ~ ws ~ "else" ~ ws ~ variable ~ ws ~ "end").map { case (condition, thenBranch, elseBranch) => IfStmt(condition, thenBranch, elseBranch) })

  def statementDerivationRule[$: P]: P[StatementDerivationRule] = P(
    "(Gamma, Delta, Context)" ~/ ws ~/ "|-" ~/ ws ~ statement ~ ws ~ "::" ~ ws ~ expressionRules
  ).map { case (statement, rules) => StatementDerivationRule(statement, rules) }

  def expressionDerivationRule[$: P]: P[ExpressionDerivationRule] = P(
    "(Gamma, Delta)" ~/ ws ~/ "|-" ~/ ws ~ expression ~ ws ~ "::" ~/ ws ~ expressionRules
  ).map { case (expr, rules) => ExpressionDerivationRule(expr, rules) }

  def expressionRules[$: P]: P[Seq[Rule]] = P("[" ~ expressionRule.rep(sep = ",") ~ ws ~ "]")

  def expressionRule[$: P]: P[Rule] = P((ws ~ condition ~ ws).rep(sep = "&&") ~ ws ~ "=>" ~ (ws ~ conclusion ~ ws).rep(1, sep = "&&")).map { case (conds, conclusion) =>
    Rule(conds, conclusion) // Placeholder, replace with actual rule creation logic
  }

  def hyperCollectionResult[$: P]: P[HyperCollectionResult] = P("H").map(_ => HyperCollectionResult())
  def deltaCollectionResult[$: P]: P[DeltaCollectionResult] = P("D").map(_ => DeltaCollectionResult())
  def context[$: P]: P[ProgramContext]                      = P("Context").map(_ => ProgramContext())
  def gamma[$: P]: P[Gamma]                                 = P("Gamma").map(_ => Gamma())
  def delta[$: P]: P[Delta]                                 = P("Delta").map(_ => Delta())
  def gammaResult[$: P]: P[GammaResult]                     = P("Gamma'").map(_ => GammaResult())
  def deltaResult[$: P]: P[DeltaResult]                     = P("Delta'").map(_ => DeltaResult())

  def hyperTypeMapping[$: P]: P[Mapping] = P(deriveHyperType | gamma)
  def deltaTypeMapping[$: P]: P[Mapping] = P(deriveDeltaType | delta)

  def mapping[$: P]: P[Mapping] = P(deltaResult | deriveHyperType | deriveDeltaType | deltaTypeCheck | gammaResult | gamma | delta | deltaCollectionResult)

  def mappingAccess[$: P]: P[MappingAccess] = P(mapping ~ "(" ~ variable ~ ")").map(x => MappingAccess(x._1, x._2))

  def set[$: P]: P[Set] = P(setWithoutElement | mappingAccess | hyperTypeCheck | hyperCollectionResult | context)

  def hyperTypeCheck[$: P]: P[HyperTypeCheck]    = P("H" ~ "[" ~ HypraParser.progVar ~ "](" ~ gamma ~ "," ~ delta ~ ")").map { case (id, gamma, delta) => HyperTypeCheck(id, gamma, delta) }
  def deltaTypeCheck[$: P]: P[DeltaTypeCheck]    = P("D" ~ "[" ~ HypraParser.progVar ~ "](" ~ gamma ~ "," ~ delta ~ ")").map { case (id, gamma, delta) => DeltaTypeCheck(id, gamma, delta) }
  def deriveHyperType[$: P]: P[DeriveHyperType]  = P("DH" ~ "[" ~ HypraParser.progVar ~ "](" ~ hyperTypeMapping ~ "," ~ deltaTypeMapping ~ "," ~ set ~ ")").map { case (id, gamma, delta, context) => DeriveHyperType(id, gamma, delta, context) }
  def deriveDeltaType[$: P]: P[DeriveDeltaType]  = P("DD" ~ "[" ~ HypraParser.progVar ~ "](" ~ hyperTypeMapping ~ "," ~ deltaTypeMapping ~ "," ~ set ~ ")").map { case (id, gamma, delta, context) => DeriveDeltaType(id, gamma, delta, context) }
  def setWithoutElement[$: P]: P[WithoutElement] = P("(" ~ set ~ ws ~ "\\" ~ element ~ ws ~ ")").map { case (set, elem) => WithoutElement(set, elem) }
  def condition[$: P]: P[Condition]              = P(equal | inSet | inMapping | arithCondition | boolCondition | negatedCondition)

  def arithCondition[$: P]: P[ArithCondition] = P(
    variable ~ ws ~ comparator ~ ws ~ CharIn("0-9").rep(1).!.map(_.toInt)
  ).map { case (variable, comp, value) => ArithCondition(variable, comp, value) }

  def boolCondition[$: P]: P[BoolCondition] = P(
    variable
  ).map { variable => BoolCondition(variable) }

  def negatedCondition[$: P]: P[Condition] = P(
    "!" ~ ws ~ "(" ~ condition ~ ")"
  ).map { case cond => NotOperator(cond) }

  def comparator[$: P]: P[String] = P(">" | "<" | ">=" | "<=" | "==" | "!=").!

  def inSet[$: P]: P[InSet]         = P(element ~ ws ~ "in" ~ ws ~ set).map { case (elem, set) => InSet(elem, set) }
  def inMapping[$: P]: P[InMapping] = P(element ~ ws ~ "in" ~ ws ~ mapping).map { case (elem, mapping) => InMapping(elem, mapping) }
  def element[$: P]: P[Element]     = P(variable | hyperType)
  def equal[$: P]: P[Equal]         = P(element ~ ws ~ "==" ~ ws ~ element).map { case (lhs, rhs) => Equal(lhs, rhs) }

  def variable[_: P]: P[Id] = {
    import fastparse.NoWhitespace._
    P(CharIn("a-z").! ~ CharIn("a-zA-Z0-9").rep.!).map { case (first, rest) => { val string = first + rest; Id(string) } }
  }

  def hyperType[$: P]: P[HyperType]             = P(hyperTypeWithSetArgs | hyperTypeWithListArgs | simpleHyperType)
  def simpleHyperType[$: P]: P[SimpleHyperType] = P(CharIn("A-Z").rep(1).!.map(SimpleHyperType))
  def hyperTypeWithSetArgs[$: P]                = P(simpleHyperType ~ "{" ~ element.rep(1, sep = ",") ~ "}").map { case (name, args) =>
    HyperTypeWithSetArgs(name, args.toSet)
  }
  def hyperTypeWithListArgs[$: P] = P(simpleHyperType ~ "[" ~ element.rep(1, sep = ",") ~ "]").map { case (name, args) =>
    HyperTypeWithListArgs(name, args.toSeq)
  }

  def conclusion[$: P]: P[Conclusion] = P(mapEquals | addToSet | setEquals)
  def addToSet[$: P]: P[AddToSet]     = P(element ~ ws ~ "addTo" ~ ws ~ set).map { case (elem, set) => AddToSet(elem, set) }
  def setEquals[$: P]: P[SetEquals]   = P(set ~ ws ~ "=" ~ ws ~ set).map { case (set1, set2) => SetEquals(set1, set2) }
  def mapEquals[$: P]: P[MapEquals]   = P(mapping ~ ws ~ "=" ~ ws ~ mapping).map { case (map1, map2) => MapEquals(map1, map2) }

  def ws[$: P]: P[Unit]               = P(CharsWhileIn(" \r\n\t").rep)
  def newlineSeparator[$: P]: P[Unit] = P(CharsWhileIn(" \t").? ~ ("\r\n" | "\n").rep(1))

}
