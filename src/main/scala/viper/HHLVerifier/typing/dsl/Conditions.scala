package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}

trait Condition extends CollectVariables {

  def check(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Boolean
}

case class InSet(elem: Element, set: Set) extends Condition {

  override def check(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Boolean = {
    val indexedSet = set match {
      case HyperTypeCheck(id, _, _) => {
        val (gamma, delta) = (context.gamma, context.delta)
        val subExpression  = context.varMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        context.typeSystem.deriveExpression(gamma, delta, subExpression, Map()).hyperTypeCollection
      }
      case _: Set => throw new Exception("Not implemented yet")
    }

    val elementIndexed = applyIndexed.applyIndexed(ruleCheckContext.variables, elem)

    indexedSet.hypertypes.contains(elementIndexed.asInstanceOf[HyperType])
  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

}

case class ArithCondition(variable: Id, op: String, right: Int) extends Condition {

  def check(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Boolean = {
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

  override def variables: ScalaSet[Id] = Set(variable)

}

case class BoolCondition(variable: Id) extends Condition {

  def check(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Boolean = {
    val expression = context.varMapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
    expression match {
      case BoolLit(value) => value
      case _              => throw new Exception("BoolCondition can only be checked against BoolLit expressions")
    }
  }

  override def variables: ScalaSet[Id] = Set(variable)

}

case class NotOperator(condition: Condition) extends Condition {

  def check(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Boolean = {
    !condition.check(context, ruleCheckContext)
  }

  override def variables: ScalaSet[Id] = condition.variables
}
