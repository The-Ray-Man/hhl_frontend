package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.silicon.state.terms.BinaryOp
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.silicon.state.terms.UnaryOp
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.dsl.Parser.hyperType
import viper.HHLVerifier.typing.dsl.Parser.setEquals
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.ast.Stmt
import scala.collection.View.Empty
import viper.carbon.boogie.All.apply
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.CombExpr
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.HyperTypeChecker.getAssignedVariables
import viper.HHLVerifier.typing.dsl.ast._
import buildinfo.BuildInfo.name

