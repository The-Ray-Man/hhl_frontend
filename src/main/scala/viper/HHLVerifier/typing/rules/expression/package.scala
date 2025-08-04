package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.rules.expression.IdentifierDerivationRule
import viper.HHLVerifier.typing.rules.expression.NumericalDerivationRule
import viper.HHLVerifier.typing.rules.expression.BooleanDerivationRule
import viper.HHLVerifier.typing.rules.expression.LengthDerivationRule
import viper.HHLVerifier.typing.rules.expression.LookupDerivationRule
import viper.HHLVerifier.typing.rules.expression.MethodDerivationRule
import viper.HHLVerifier.typing.rules.expression.UnaryExpressionDerivationRule
import viper.HHLVerifier.typing.rules.expression.BinaryExpressionDerivationRule
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.LengthExpr

package object expression {
  
}
case class ExpressionSystem(
    identifierRule: IdentifierDerivationRule = IdentifierDerivationRule(),
    constRule: NumericalDerivationRule = NumericalDerivationRule(),
    boolRule: BooleanDerivationRule = BooleanDerivationRule(),
    binaryRule: BinaryExpressionDerivationRule = BinaryExpressionDerivationRule(),
    unaryRule: UnaryExpressionDerivationRule = UnaryExpressionDerivationRule(),
    methodCallRule: MethodDerivationRule = MethodDerivationRule(),
    lookupRule: LookupDerivationRule = LookupDerivationRule(),
    lengthRule: LengthDerivationRule = LengthDerivationRule(),
) {
    def derive(e: Expr, mapping: HyperMapping) : (HyperTypeCollection, DeltaCollection) = {
        e match {
            case id@Id(_) => identifierRule.derive(this, id, mapping)
            case num@Num(_) => constRule.derive(this, num, mapping)
            case bool@BoolLit(_) => boolRule.derive(this, bool, mapping)
            case binaryExpr@BinaryExpr(_, _, _) => binaryRule.derive(this, binaryExpr, mapping)
            case unaryExpr@UnaryExpr(_, _) => unaryRule.derive(this, unaryExpr, mapping)
            case methodCall@MethodCallExpr(_, _) => methodCallRule.derive(this, methodCall, mapping)
            case lookup@LookupExpr(_, _) => lookupRule.derive(this, lookup, mapping)
            case lengthExpr@LengthExpr(_) => lengthRule.derive(this, lengthExpr, mapping)
            case _ => throw new Exception("Cannot derive expression: " + e)
        }
    }
}