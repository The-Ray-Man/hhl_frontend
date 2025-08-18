package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}

trait Condition extends CollectVariables {

  def check(context: ExpressionDerivationContext): Boolean
}

case class InSet(elem: Element, set: Set) extends Condition {

  override def check(context: ExpressionDerivationContext): Boolean = {
    set match {
      case HyperTypeCheck(id, _, _) => {
        val (gamma, delta) = (context.gamma, context.delta)
        val subExpression  = context.varMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        context.typeSystem.deriveExpression(gamma, delta, subExpression, Map()).hyperTypeCollection.hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case MappingAccess(DeltaTypeCheck(sub, _, _), id) => {
        val subExpression     = context.varMapping.getOrElse(sub, throw new Exception(s"Variable $sub not found in variable mapping"))
        val subExpressionType = context.typeSystem.deriveExpression(context.gamma, context.delta, subExpression, Map())
        val indexedSet        = subExpressionType.deltaCollection.mapping.getOrElse(id.name, throw new Exception(s"Variable $id not found in delta mapping"))
        indexedSet.hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case _: Set => throw new Exception("Not implemented yet")
    }

  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

}

case class InMapping(elem: Element, mapping: Mapping) extends Condition {

  override def check(context: ExpressionDerivationContext): Boolean = {
    (elem, mapping) match {
      case (Id(name), Delta())                   => context.delta.collection.contains(name)
      case (Id(name), Gamma())                   => context.gamma.mapping.contains(name)
      case (Id(name), DeltaTypeCheck(sub, _, _)) => {
        val subExpression = context.varMapping.getOrElse(sub, throw new Exception(s"Variable $sub not found in variable mapping"))
        context.typeSystem.deriveExpression(context.gamma, context.delta, subExpression, Map()).deltaCollection.mapping.contains(name)
      }
      case (_, _) => throw new Exception("Unsupported mapping type for InMapping condition")

    }
  }

  override def variables: ScalaSet[Id] = elem.variables ++ mapping.variables

}

case class ArithCondition(variable: Id, op: String, right: Int) extends Condition {

  def check(context: ExpressionDerivationContext): Boolean = {
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

  def check(context: ExpressionDerivationContext): Boolean = {
    val expression = context.varMapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
    expression match {
      case BoolLit(value) => value
      case _              => throw new Exception("BoolCondition can only be checked against BoolLit expressions")
    }
  }

  override def variables: ScalaSet[Id] = Set(variable)

}

case class NotOperator(condition: Condition) extends Condition {

  def check(context: ExpressionDerivationContext): Boolean = {
    !condition.check(context)
  }

  override def variables: ScalaSet[Id] = condition.variables
}
