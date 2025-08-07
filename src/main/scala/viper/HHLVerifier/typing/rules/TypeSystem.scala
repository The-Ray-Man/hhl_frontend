package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule
import viper.HHLVerifier.typing.dsl.SpecificationUtil

case class TypeSystem(
    statementTypeSystem: StatementTypeSystem = null,
    expressionTypeSystem: ExpressionTypeSystem = null
) {


  def checkSoundness() : Boolean = {
    false
  }
}

case class StatementTypeSystem()

object TypeSystem {
  def loadTypeSystem(paths: Seq[String]): TypeSystem = {
    val specifications = paths.map(path => {
        val fileContent   = scala.io.Source.fromFile(path).getLines().mkString("\n")
        val res           = fastparse.parse(fileContent, viper.HHLVerifier.typing.dsl.Parser.specification(_))
        res match {
          case fastparse.Parsed.Success(value, _) => value
          case failure: fastparse.Parsed.Failure  => throw new Exception(s"Failed to parse type system: ${failure.msg}")
        }
    })

    SpecificationUtil.combineSpecifications(specifications).toTypeSystem()
  }
}
