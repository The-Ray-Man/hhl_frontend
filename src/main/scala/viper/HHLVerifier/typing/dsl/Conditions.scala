package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.HyperTypeChecker.getAssignedVariables

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
    val indexedElem = context.applyIndexed(elem)
    set match {
      case _: HyperTypeCheck => {
        val derivedCollection = DeriveArgsUtils(context).getHyperTypeCollection(set)
        derivedCollection.hypertypes.contains(indexedElem.asInstanceOf[HyperType])
      }
      case MappingAccess(d: DeltaTypeCheck, id) => {
        val derivedDeltaCollection = DeriveArgsUtils(context).getDeltaCollection(d)
        val indexedId              = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val indexedSet             = derivedDeltaCollection.mapping.getOrElse(indexedId.name, throw new Exception(s"Variable $indexedId not found in delta mapping"))
        indexedSet.hypertypes.contains(indexedElem.asInstanceOf[HyperType])
      }
      case MappingAccess(Gamma(), id) => {
        val indexedValue = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        context.gamma.mapping.getOrElse(indexedValue.name, return false).hypertypes.contains(indexedElem.asInstanceOf[HyperType])
      }
      case MappingAccess(d: DeriveHyperType, id) => {
        val derivedGamma = DeriveArgsUtils(context).getHyperMapping(d)
        val indexedId    = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val indexedSet   = derivedGamma.get(indexedId.name)
        indexedSet.hypertypes.contains(indexedElem.asInstanceOf[HyperType])
      }
      case varSet: Variables => {
        val variable    = indexedElem.asInstanceOf[Id]
        val variableSet = DeriveArgsUtils(context).getVariableSet(varSet)
        variableSet.contains(variable)
      }
      case MappingAccess(MappingAccess(mapping, var1), var2) => {
        val indexedVar1     = context.varExprMapping.getOrElse(var1, var1).asInstanceOf[Id]
        val indexedVar2     = context.varExprMapping.getOrElse(var2, var2).asInstanceOf[Id]
        val getDeltaMapping = DeriveArgsUtils(context).getDeltaMapping(mapping)
        getDeltaMapping.collection.getOrElse(indexedVar1.name, return false).mapping.getOrElse(indexedVar2.name, return false).hypertypes.contains(indexedElem.asInstanceOf[HyperType])
      }
      case AssignedVariables(stmt) => {
        val actualStmt       = context.varStmtMapping.getOrElse(stmt, throw new Exception("Could not index stmt"))
        val changedVariables = getAssignedVariables(actualStmt)
        val variable         = elem.asInstanceOf[Id]
        changedVariables.contains(variable)
      }
      case AllParameters() => {
        val vars = context.typeSystem.allParams
        vars.contains(indexedElem.asInstanceOf[Id])
      }
      case _: Set => throw new Exception("Not implemented yet " + set.getClass().getSimpleName())
    }

  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

}

case class InMapping(elem: Element, mapping: Mapping) extends Condition {

  override def check(context: Context, expression: Boolean): Boolean = {
    val indexedElem = context.applyIndexed(elem)
    (indexedElem, mapping) match {
      case (Id(name), Delta())         => context.delta.collection.contains(name)
      case (Id(name), Gamma())         => context.gamma.mapping.contains(name)
      case (id: Id, d: DeltaTypeCheck) => {
        val indexedId         = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val derivedCollection = DeriveArgsUtils(context).getDeltaCollection(d)
        derivedCollection.mapping.contains(indexedId.name)
      }
      case (id: Id, d: MappingAccess) => {
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
