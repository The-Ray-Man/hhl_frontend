package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule

case class TypeSystem(
    statementTypeSystem: StatementTypeSystem = null,
    ExpressionTypeSystem: ExpressionTypeSystem = null
) {}

case class StatementTypeSystem()

object TypeSystem {
  def loadTypeSystem(path: String): TypeSystem = {
    val fileContent   = scala.io.Source.fromFile(path).getLines().mkString("\n")
    val res           = fastparse.parse(fileContent, viper.HHLVerifier.typing.dsl.Parser.specification(_))
    val specification = res match {
      case fastparse.Parsed.Success(value, _) => value
      case failure: fastparse.Parsed.Failure  => throw new Exception(s"Failed to parse type system: ${failure.msg}")
    }
    specification.toTypeSystem()
  }
}
