package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.TypeSystem
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

  val wrappedRules: Seq[typing.rules.RuleWrapper] = rules.map(rule => wrapRule(rule))

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

  def wrapRule(rule: Rule): typing.rules.RuleWrapper = {
    val allVariables        = rule.conditions.flatMap(_.variables).toSet
    val capturedVariables   = typing.HyperTypeChecker.getVariables(expr).toSet
    val freeVariables       = allVariables -- capturedVariables
    val freeVariableMapping = freeVariables.zipWithIndex.toMap
    val indexedRule         = ToIndexed.toIndexedVariable(freeVariableMapping, rule)
    if (freeVariables.isEmpty) {
      typing.rules.EmptyWrapper(indexedRule)
    } else {
      typing.rules.ForanyVariableWrapper(freeVariables.size, indexedRule)
    }
  }

  def derive(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]): typing.rules.ExpressionDerivationResult = {
    val context     = typing.rules.ExpressionDerivationContext(typeSystem, expr, gamma, delta, variableMapping)
    val emptyResult = typing.rules.ExpressionDerivationResult(typing.HyperTypeCollection(Set.empty), typing.DeltaCollection(Map.empty))
    wrappedRules.foldLeft(emptyResult) { (acc, rule) =>
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
trait Mapping extends ConclusionInfo
trait Set     extends ConclusionInfo with CollectVariables

case class HyperTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Set {

  override def isHyperTypeConclusion(): Boolean = false

  override def variables: immutable.Set[Id] = immutable.Set.empty[Id]

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

  override def isHyperTypeConclusion(): Boolean = false

}
case class Gamma() extends Mapping {

  override def isHyperTypeConclusion(): Boolean = true

}
case class Delta() extends Mapping {

  override def isHyperTypeConclusion(): Boolean = false

}

trait Element extends CollectVariables {}

abstract class HyperType extends Element {
  override def equals(obj: Any): Boolean
  def semantics(id: Id): Expr = throw new Exception("Semantics not defined for HyperType: " + this.getClass.getSimpleName)
}

case class SimpleHyperType(name: String) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case SimpleHyperType(otherName) => name == otherName
    case _                          => false
  }

  override def variables: scala.collection.immutable.Set[Id] = scala.collection.immutable.Set.empty[Id]
}
case class HyperTypeWithSetArgs(name: SimpleHyperType, args: scala.collection.immutable.Set[Element]) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithSetArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                          => false
  }

  override def variables: scala.collection.immutable.Set[Id] = args.flatMap(_.variables)
}
case class HyperTypeWithListArgs(name: SimpleHyperType, args: Seq[Element]) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithListArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                           => false
  }

  override def variables: scala.collection.immutable.Set[Id] = args.flatMap(_.variables).toSet
}

// Building Blocks for Conditions/Conclusion
trait Condition extends CollectVariables {

  def check(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext): Boolean
}

case class InSet(elem: Element, set: Set) extends Condition {

  override def check(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext): Boolean = {
    val indexedSet = set match {
      case HyperTypeCheck(id, _, _) => {
        val (gamma, delta) = (context.gamma, context.delta)
        val subExpression  = context.varMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        context.typeSystem.deriveExpression(gamma, delta, subExpression, Map()).hyperTypeCollection
      } // Todo gamma, delta collection should be infered.
      case _: Set => throw new Exception("Not implemented yet")
    }

    val elementIndexed = applyIndexed.applyIndexed(ruleCheckContext.variables, elem)

    indexedSet.hypertypes.contains(elementIndexed.asInstanceOf[HyperType])
  }
  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

}

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
  def apply(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext, result: typing.rules.ExpressionDerivationResult): typing.rules.ExpressionDerivationResult
}

case class AddToSet(elem: Element, set: Set) extends Conclusion {

  def apply(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext, result: typing.rules.ExpressionDerivationResult): typing.rules.ExpressionDerivationResult = {
    set match {
      case HyperCollectionResult() => {
        val updatedElem = applyIndexed.applyIndexed(ruleCheckContext.variables, elem).asInstanceOf[HyperType]
        typing.rules.ExpressionDerivationResult(
          hyperTypeCollection = result.hyperTypeCollection.add(updatedElem),
          deltaCollection = result.deltaCollection
        )
      }
      case _: Set => throw new Exception("Not implemented yet")
    }
  }
  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}

case class SetEquals(set1: Set, set2: Set) extends Conclusion {

  def hyperTypeResultToGammaLookup(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext, result: typing.rules.ExpressionDerivationResult, index: Id): typing.rules.ExpressionDerivationResult = {
    context.varMapping.getOrElse(index, throw new Exception(s"Variable $index not found in variable mapping")) match {
      case Id(name) => {
        val hyperTypes = context.gamma.getUnsafe(name)
        typing.rules.ExpressionDerivationResult(
          hyperTypeCollection = hyperTypes,
          deltaCollection = result.deltaCollection
        )
      }
      case _ => throw new Exception("Expected Id for index in MappingAccess")
    }
  }

  override def apply(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext, result: typing.rules.ExpressionDerivationResult): typing.rules.ExpressionDerivationResult = {
    (set1, set2) match {
      case (HyperCollectionResult(), MappingAccess(Gamma(), index)) => hyperTypeResultToGammaLookup(context, ruleCheckContext, result, index)
      case (MappingAccess(Gamma(), index), HyperCollectionResult()) => hyperTypeResultToGammaLookup(context, ruleCheckContext, result, index)
      case _                                                        => throw new Exception("Not yet implemented")
    }
  }

  override def variables: immutable.Set[Id] = set1.variables ++ set2.variables

  override def isHyperTypeConclusion(): Boolean = {
    val res1 = set1.isHyperTypeConclusion()
    val res2 = set2.isHyperTypeConclusion()
    if (res1 != res2) {
      throw new Exception("SetEquals conclusion must have both sets of the same type")
    }
    res1
  }

}

case class ArithCondition(variable: Id, op: String, right: Int) extends Condition {

  def check(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext): Boolean = {
    val expression = context.varMapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
    expression match {
      case Num(value) => {
        op match {
          case ">"  => value > right
          case "<"  => value < right
          case ">=" => value >= right
          case "<=" => value <= right
          case "==" => value == right
          case "!=" => value != right
          case _    => throw new Exception("Unsupported comparator: " + op)
        }
      }
      case _ => throw new Exception("ArithCondition can only be checked against Num expressions")
    }
  }

  override def variables: immutable.Set[Id] = Set(variable)

}

case class BoolCondition(variable: Id) extends Condition {

  def check(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext): Boolean = {
    val expression = context.varMapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
    expression match {
      case BoolLit(value) => value
      case _              => throw new Exception("BoolCondition can only be checked against BoolLit expressions")
    }
  }

  override def variables: immutable.Set[Id] = Set(variable)

}

case class NotOperator(condition: Condition) extends Condition {

  def check(context: typing.rules.ExpressionDerivationContext, ruleCheckContext: typing.rules.RuleCheckContext): Boolean = {
    !condition.check(context, ruleCheckContext)
  }

  override def variables: immutable.Set[Id] = condition.variables
}
