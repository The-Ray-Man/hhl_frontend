package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.dsl.SpecificationUtil
import viper.HHLVerifier.typing.dsl.HyperTypeDeclaration
import viper.HHLVerifier.typing.dsl
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping

case class TypeSystem(
    statementTypeSystem: StatementTypeSystem = null,
    expressionTypeSystem: Seq[dsl.ExpressionDerivationRule] = null,
    hyperTypeDeclaration : Seq[HyperTypeDeclaration] = Seq.empty[HyperTypeDeclaration]
) {


  def checkSoundness() : Boolean = {
    false
  }

  def deriveExpression(gamma: HyperMapping, delta: DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): (HyperTypeCollection, DeltaCollection) = {
    val applicableRules = expressionTypeSystem.map(rule => (rule, rule.isApplicableTo(expr))).filter(_._2.isDefined).map(rule =>(rule._1, rule._2.get))
    if (applicableRules.isEmpty || applicableRules.length > 1) {
      throw new Exception(s"There are ${applicableRules.length} applicable rules for expression $expr")
    }
    val rule = applicableRules.head
    // val (newGamma, newDelta) = rule._1.derive(this, gamma, delta, expr, rule._2)
    // (newGamma, newDelta)
    (null, null)
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
