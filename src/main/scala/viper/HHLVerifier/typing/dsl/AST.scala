package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing
import scala.collection.immutable
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.LengthExpr

case class Specification(hypertypeDeclaration: Seq[HyperTypeDeclaration], derivationRules: Seq[DerivationRule]) {

  def toTypeSystem(): TypeSystem = {
    val expressionRules = derivationRules.filter(_.isInstanceOf[ExpressionDerivationRule]).map(_.asInstanceOf[ExpressionDerivationRule])

    val typeSystem = TypeSystem(
      statementTypeSystem = null,
      expressionTypeSystem = expressionRules,
      hyperTypeDeclaration = hypertypeDeclaration
    )
    typeSystem
  }
}

case class HyperTypeDeclaration(variable: Id, hty: HyperType, definition: Expr) {}

trait DerivationRule

case class ExpressionDerivationRule(expr: Expr, rules: Seq[Rule]) extends DerivationRule {

  val wrappedRules: Seq[RuleWrapper] = rules.map(rule => wrapRule(rule))

  def isApplicableTo(toCheckExpression: Expr): Option[Map[Id, Expr]] = {
    // Check if the expression matches the rule's expression
    // Returns a mapping that maps the "template ids" to the Id in the expression.
    (expr, toCheckExpression) match {
      case (BinaryExpr(left, op, right), BinaryExpr(leftCheck, opCheck, rightCheck)) if op == opCheck => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (UnaryExpr(op, inner), UnaryExpr(opCheck, innerCheck)) if op == opCheck                    => Some(matchHelper(Seq((inner, innerCheck))))
      case (Id("n"), Num(value))                                                                      => Some(Map(Id("n") -> Num(value)))
      case (Id("var"), Id(name))                                                                      => Some(Map(Id("var") -> Id(name)))
      case (Id("b"), BoolLit(name))                                                                   => Some(Map(Id("b") -> BoolLit(name)))
      case (ImpliesExpr(left, right), ImpliesExpr(leftCheck, rightCheck))                             => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (_, MethodCallExpr(_, _))                                                                  => throw new Exception("Method calls are not supported in expression derivation rules")
      case (LookupExpr(dataStructure, index), LookupExpr(dataStructureCheck, indexCheck))             => Some(matchHelper(Seq((dataStructure, dataStructureCheck), (index, indexCheck))))
      case (LengthExpr(dataStructure), LengthExpr(dataStructureCheck))                                => Some(matchHelper(Seq((dataStructure, dataStructureCheck))))
      case _                                                                                          => None
    }
  }

  def matchHelper(matching: Seq[(Expr, Expr)]): Map[Id, Expr] = {
    matching.foldLeft(Map.empty[Id, Expr]) { (acc, pair) =>
      (pair._1, pair._2) match {
        case (id: Id, expr: Expr) => acc + (id -> expr)
        case _                    => throw new Exception("The template expression can not be recursive")
      }
    }
  }

  def wrapRule(rule: Rule): RuleWrapper = {
    val allVariables        = rule.conditions.flatMap(_.variables).toSet ++ rule.conclusions.flatMap(_.variables).toSet
    val capturedVariables   = typing.HyperTypeChecker.getVariables(expr).toSet
    val freeVariables       = allVariables -- capturedVariables
    val freeVariableMapping = freeVariables.zipWithIndex.toMap
    val indexedRule         = ToIndexed.toIndexedVariable(freeVariableMapping, rule)
    if (freeVariables.isEmpty) {
      EmptyWrapper(indexedRule)
    } else {
      ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): ExpressionDerivationResult = {
    val context     = ExpressionDerivationContext(typeSystem, expr, gamma, delta, variableMapping)
    val emptyResult = ExpressionDerivationResult(typing.HyperTypeCollection(Set.empty), typing.DeltaCollection(Map.empty))
    wrappedRules.foldLeft(emptyResult) { (acc, rule) =>
      println(acc)
      rule.apply(context, acc)
    }

  }
}

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion]) {}

trait ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
}

trait CollectVariables {
  def variables: scala.collection.immutable.Set[Id]
}

// Building Blocks for Condition and Conclusion
trait Mapping extends ConclusionInfo with CollectVariables
trait Set     extends ConclusionInfo with CollectVariables

case class HyperTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Set {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

}

case class DeltaTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Mapping {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set(expr)
}

case class HyperCollectionResult() extends Set {

  override def isHyperTypeConclusion(): Boolean = true
  override def variables: immutable.Set[Id]     = immutable.Set.empty[Id]
}
case class MappingAccess(mapping: Mapping, id: Id) extends Set {

  override def isHyperTypeConclusion(): Boolean = mapping.isHyperTypeConclusion()
  override def variables: immutable.Set[Id]     = scala.collection.immutable.Set(id)
}

case class DeltaCollectionResult() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]


  override def isHyperTypeConclusion(): Boolean = false

}
case class Gamma() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]


  override def isHyperTypeConclusion(): Boolean = true

}
case class Delta() extends Mapping {

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]


  override def isHyperTypeConclusion(): Boolean = false

}
