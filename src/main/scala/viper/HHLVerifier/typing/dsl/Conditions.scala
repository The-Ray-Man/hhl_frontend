package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables

trait Condition extends CollectVariables {

  def check(context: Context, expression: Boolean): Boolean
}

case class Equal(lhs: Element, rhs: Element) extends Condition {

  override def variables: ScalaSet[Id] = lhs.variables ++ rhs.variables

  override def check(context: Context, expression: Boolean): Boolean = {
    (lhs, rhs) match {
      case (SimpleHyperType(lhsName), SimpleHyperType(rhsName))                               => lhsName == rhsName
      case (HyperTypeWithListArgs(lhsName, lhsArgs), HyperTypeWithListArgs(rhsName, rhsArgs)) => lhsName == rhsName && lhsArgs == rhsArgs
      case (HyperTypeWithSetArgs(lhsName, lhsArgs), HyperTypeWithSetArgs(rhsName, rhsArgs))   => lhsName == rhsName && lhsArgs == rhsArgs
      case (lhs @ Id(_), rhs @ Id(_))                                                         => {
        val lhsIndexed = if (context.varExprMapping.contains(lhs)) { context.varExprMapping.getOrElse(lhs, lhs) }
        else context.varStmtMapping.getOrElse(lhs, lhs)
        val rhsIndexed = if (context.varExprMapping.contains(rhs)) { context.varExprMapping.getOrElse(rhs, rhs) }
        else context.varStmtMapping.getOrElse(rhs, rhs)
        lhsIndexed == rhsIndexed
      }
      case (a, b) => {
        if (a.getClass() != b.getClass()) {
          false
        } else {
          throw new Exception("This comparison is not implemented")
        }
      }
    }
  }
}

case class InSet(elem: Element, set: Set) extends Condition {

  override def check(context: Context, expression: Boolean): Boolean = {
    set match {
      case _: HyperTypeCheck => {
        val derivedCollection = DeriveArgsUtils(context).getHyperTypeCollection(set)
        derivedCollection.hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case MappingAccess(d: DeltaTypeCheck, id) => {
        val derivedDeltaCollection = DeriveArgsUtils(context).getDeltaCollection(d)
        val indexedId              = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val indexedSet             = derivedDeltaCollection.mapping.getOrElse(indexedId.name, throw new Exception(s"Variable $id not found in delta mapping"))
        indexedSet.hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case MappingAccess(Gamma(), id) => {
        val indexedValue = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        context.gamma.mapping.getOrElse(indexedValue.name, return false).hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case MappingAccess(d: DeriveHyperType, id) => {
        val derivedGamma = DeriveArgsUtils(context).getHyperMapping(d)
        val indexedId    = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val indexedSet   = derivedGamma.get(indexedId.name)
        indexedSet.hypertypes.contains(elem.asInstanceOf[HyperType])
      }
      case Variables(content) => {
        val variable = elem.asInstanceOf[Id]
        var existingVariables = getVariables(context.getExprById(content))
        if (context.isInstanceOf[StatementDerivationContext]) {
          existingVariables ++= getVariables(context.getStmtById(content))
        }
        existingVariables.contains(variable)
      }
      case _: Set => throw new Exception("Not implemented yet " + set.getClass().getSimpleName())
    }

  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

}

case class InMapping(elem: Element, mapping: Mapping) extends Condition {

  override def check(context: Context, expression: Boolean): Boolean = {
    (elem, mapping) match {
      case (Id(name), Delta())         => context.delta.collection.contains(name)
      case (Id(name), Gamma())         => context.gamma.mapping.contains(name)
      case (id: Id, d: DeltaTypeCheck) => {
        val indexedId         = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val derivedCollection = DeriveArgsUtils(context).getDeltaCollection(d)
        derivedCollection.mapping.contains(indexedId.name)
      }
      case (_, _) => throw new Exception("Unsupported mapping type for InMapping condition")

    }
  }

  override def variables: ScalaSet[Id] = elem.variables ++ mapping.variables

}

case class ArithCondition(variable: Id, op: String, right: Int) extends Condition {

  def check(context: Context, expression: Boolean): Boolean = {
    val expression = context.getExprById(variable)
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

  def check(context: Context, expression: Boolean): Boolean = {
    val expression = context.getExprById(variable)
    expression match {
      case BoolLit(value) => value
      case _              => throw new Exception("BoolCondition can only be checked against BoolLit expressions")
    }
  }

  override def variables: ScalaSet[Id] = Set(variable)

}

case class NotOperator(condition: Condition) extends Condition {

  def check(context: Context, expression: Boolean): Boolean = {
    !condition.check(context, expression)
  }

  override def variables: ScalaSet[Id] = condition.variables
}
