package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.TypeSystem
import viper.HHLVerifier.typing
import scala.collection.immutable
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr

case class Specification(derivationRules: Seq[DerivationRule]) {

  def toTypeSystem(): TypeSystem = {

    val rules = derivationRules.map { rule =>
      rule match {
        case rule: ExpressionDerivationRule =>
          rule.toTypeSystem()
        case _ =>
          throw new Exception("Unsupported derivation rule type")
      }
    }
    println(rules)
    null
  }
}

trait DerivationRule

case class ExpressionDerivationRule(op: String, inputs: Seq[(HyperCollection, DeltaCollection)], rules: Seq[Rule]) extends DerivationRule {
  def toTypeSystem(): typing.rules.ExpressionDerivationRule = {
    val hyperTypeRules = Seq.empty[typing.rules.RuleWrapper[typing.rules.HyperTypeConclusion]]
    val deltaRules     = Seq.empty[typing.rules.RuleWrapper[typing.rules.DeltaConclusion]]
    val res            = rules.map(rule => rule.toTypeSystem()).fold((hyperTypeRules, deltaRules))((acc, current) => (acc._1 ++ current._1, acc._2 ++ current._2))
    op match {
      case "+" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.AdditionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.AdditionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.AdditionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
    }
  }
}

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion]) {
  def toTypeSystem(): (Seq[typing.rules.RuleWrapper[typing.rules.HyperTypeConclusion]], Seq[typing.rules.RuleWrapper[typing.rules.DeltaConclusion]]) = {
    val hyperTypeConclusions = conclusions.filter(conclusion => conclusion.isHyperTypeConclusion())
    val deltaConclusions     = conclusions.filter(conclusion => !conclusion.isHyperTypeConclusion())
    val variables            = conditions.flatMap(_.variables).toSet ++ conclusions.flatMap(_.variables).toSet
    val variableMap          = variables.zipWithIndex.toMap

    println(variables)

    (null, null) // TODO: Implement conversion to TypeSystem rules
  }
}

trait ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
}

trait CollectVariables {
  def variables: scala.collection.immutable.Set[Id]
}

// Building Blocks for Condition and Conclusion
trait Mapping extends ConclusionInfo
trait Set     extends ConclusionInfo with CollectVariables

case class HyperCollection(id: Number) extends Set {
  override def isHyperTypeConclusion(): Boolean = true
  override def variables: immutable.Set[Id]     = immutable.Set.empty[Id]
}
case class HyperCollectionResult() extends Set {
  override def isHyperTypeConclusion(): Boolean = true
  override def variables: immutable.Set[Id]     = immutable.Set.empty[Id]
}
case class MappingAccess(mapping: Mapping, id: Id) extends Set {
  override def isHyperTypeConclusion(): Boolean = mapping.isHyperTypeConclusion()
  override def variables: immutable.Set[Id]     = scala.collection.immutable.Set(id)
}

case class DeltaCollection(id: Number) extends Mapping {

  override def isHyperTypeConclusion(): Boolean = false

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

trait Element extends CollectVariables {
  def toIndexed(variableMap: Map[Id, Int]): Element = this match {
    case variable: Identifier => variable.toIndexedIdentifier(variableMap)
    case hyperType: HyperType => hyperType.toIndexedHypertype(variableMap)
    case _                    => throw new Exception("Unsupported element type for indexing")
  }
}

trait Identifier extends Element {
  def toIndexedIdentifier(variableMap: Map[Id, Int]): IndexedVariable
}

abstract class HyperType extends Element {
  override def equals(obj: Any): Boolean
  def toIndexedHypertype(variableMap: Map[Id, Int]): HyperType
  def semantics(id: Id): Expr = throw new Exception("Semantics not defined for HyperType: " + this.getClass.getSimpleName)
}

case class IndexedVariable(id: Int) extends Identifier {
  override def toIndexedIdentifier(variableMap: Map[Id, Int]): IndexedVariable = throw new Exception("IndexedVariable cannot be converted multiple times!")
  override def variables: scala.collection.immutable.Set[Id]                   = throw new Exception("IndexedVariable cannot be collected as Variable")
}

case class SimpleHyperType(name: String) extends HyperType {

  override def toIndexedHypertype(variableMap: Map[Id, Int]): HyperType = this

  override def equals(obj: Any): Boolean = obj match {
    case SimpleHyperType(otherName) => name == otherName
    case _                          => false
  }

  override def variables: scala.collection.immutable.Set[Id] = scala.collection.immutable.Set.empty[Id]
}
case class HyperTypeWithSetArgs(name: SimpleHyperType, args: scala.collection.immutable.Set[Element]) extends HyperType {

  override def toIndexedHypertype(variableMap: Map[Id, Int]): HyperType = HyperTypeWithSetArgs(name, args.map(_.toIndexed(variableMap)))

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithSetArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                          => false
  }

  override def variables: scala.collection.immutable.Set[Id] = args.flatMap(_.variables)
}
case class HyperTypeWithListArgs(name: SimpleHyperType, args: Seq[Element]) extends HyperType {

  override def toIndexedHypertype(variableMap: Map[Id, Int]): HyperType = HyperTypeWithListArgs(name, args.map(_.toIndexed(variableMap)))

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithListArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                           => false
  }

  override def variables: scala.collection.immutable.Set[Id] = args.flatMap(_.variables).toSet
}

// Building Blocks for Conditions/Conclusion
trait Condition extends CollectVariables {
  // the list is formatted like this [SideConditions, ConditionHyperType0, ConditionDeltaType0, ConditionHyperType1, ConditionDeltaType1, ...]
  // We will store this such that we save (index, Condition)
  def toTypeCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)]

}

case class InSet(elem: Element, set: Set) extends Condition {

  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

  override def toTypeCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)] = {
    set match {
      case _ @HyperCollection(id) => {
        val hyperType = elem.toIndexed(variableMap).asInstanceOf[HyperType]
        val index     = id.intValue() * 2 + 1
        Seq((index, typing.rules.ElementOf(hyperType)))
      }
      case _ => throw new Exception("Unsupported set type for InSet condition")
    }
  }

}

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
}
case class AddToSet(elem: Element, set: Set) extends Conclusion {

  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}
case class SetEquals(set1: Set, set2: Set) extends Conclusion {

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
