package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule

case class TypeSystem(
    statementTypeSystem: StatementTypeSystem = StatementTypeSystem(),
    expressionSystem: ExpressionSystem = ExpressionSystem()
) {}

case class StatementTypeSystem(
)
