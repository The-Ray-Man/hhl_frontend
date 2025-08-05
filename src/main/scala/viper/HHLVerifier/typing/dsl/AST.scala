package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.TypeSystem
import viper.HHLVerifier.typing
import scala.collection.immutable
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.rules.expression._
import viper.HHLVerifier.typing.rules.expression.binaryOp._
import viper.HHLVerifier.typing.rules.expression.unaryOp._

case class Specification(derivationRules: Seq[DerivationRule]) {

  def toTypeSystem(): TypeSystem = {

    val rules = derivationRules.map { rule =>
      rule match {
        case rule @ ExpressionDerivationRule(op, _, _) =>
          (op, rule.toTypeSystem())
        case _ =>
          throw new Exception("Unsupported derivation rule type")
      }
    }
    val expressionDerivationRules = typing.rules.ExpressionTypeSystem(
      identifierRule = SpecificationUtil.findExpressionRuleByName[IdentifierDerivationRule](rules, "var"),
      constRule = SpecificationUtil.findExpressionRuleByName[NumericalDerivationRule](rules, "n"),
      boolRule = SpecificationUtil.findExpressionRuleByName[BooleanDerivationRule](rules, "b"),
      binaryRule = BinaryExpressionDerivationRule(
        additionRule = SpecificationUtil.findExpressionRuleByName[AdditionDerivationRule](rules, "+"),
        andRule = SpecificationUtil.findExpressionRuleByName[AndDerivationRule](rules, "&&"),
        divisionRule = SpecificationUtil.findExpressionRuleByName[DivisionDerivationRule](rules, "/"),
        equalityRule = SpecificationUtil.findExpressionRuleByName[EqualityDerivationRule](rules, "=="),
        extensionRule = SpecificationUtil.findExpressionRuleByName[ExtensionDerivationRule](rules, "++"),
        greaterRule = SpecificationUtil.findExpressionRuleByName[GreaterDerivationRule](rules, ">"),
        greaterEqualRule = SpecificationUtil.findExpressionRuleByName[GreaterEqualDerivationRule](rules, ">="),
        implicationRule = SpecificationUtil.findExpressionRuleByName[ImplicationDerivationRule](rules, "==>"),
        inRule = SpecificationUtil.findExpressionRuleByName[InDerivationRule](rules, "in"),
        inequalityRule = SpecificationUtil.findExpressionRuleByName[InequalityDerivationRule](rules, "!="),
        moduloRule = SpecificationUtil.findExpressionRuleByName[ModuloDerivationRule](rules, "%"),
        multiplicationRule = SpecificationUtil.findExpressionRuleByName[MultiplicationDerivationRule](rules, "*"),
        orRule = SpecificationUtil.findExpressionRuleByName[OrDerivationRule](rules, "||"),
        setminusRule = SpecificationUtil.findExpressionRuleByName[SetminusDerivationRule](rules, "setminus"),
        smallerRule = SpecificationUtil.findExpressionRuleByName[SmallerDerivationRule](rules, "<"),
        smallerEqualRule = SpecificationUtil.findExpressionRuleByName[SmallerEqualDerivationRule](rules, "<="),
        subtractionRule = SpecificationUtil.findExpressionRuleByName[SubtractionDerivationRule](rules, "-"),
        unionRule = SpecificationUtil.findExpressionRuleByName[UnionDerivationRule](rules, "union")
      ),
      unaryRule = typing.rules.expression.UnaryExpressionDerivationRule(
        negateRule = SpecificationUtil.findExpressionRuleByName[NegateDerivationRule](rules, "negate"),
        notRule = SpecificationUtil.findExpressionRuleByName[NotDerivationRule](rules, "!")
      ),
      methodCallRule = SpecificationUtil.findExpressionRuleByName[MethodDerivationRule](rules, "methodCall"),
      lookupRule = SpecificationUtil.findExpressionRuleByName[LookupDerivationRule](rules, "lookup"),
      lengthRule = SpecificationUtil.findExpressionRuleByName[LengthDerivationRule](rules, "length")
    )

    val statementTypeSystem = typing.rules.StatementTypeSystem()

    TypeSystem(
      statementTypeSystem = statementTypeSystem,
      ExpressionTypeSystem = expressionDerivationRules
    )
  }
}

trait DerivationRule

case class ExpressionDerivationRule(op: String, inputs: Seq[(HyperCollection, DeltaCollection)], rules: Seq[Rule]) extends DerivationRule {
  def toTypeSystem(): typing.rules.ExpressionDerivationRule = {
    println("ToTypeSystem")
    val hyperTypeRules = Seq.empty[typing.rules.RuleWrapper[typing.rules.HyperTypeConclusion]]
    val deltaRules     = Seq.empty[typing.rules.RuleWrapper[typing.rules.DeltaConclusion]]
    val arity          = inputs.length
    val res            = rules.map(rule => rule.toTypeSystem(arity)).fold((hyperTypeRules, deltaRules))((acc, current) => (acc._1 ++ current._1, acc._2 ++ current._2))
    op match {
      case "+" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.AdditionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.AdditionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.AdditionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "var" => {
        val hyperCombinationFunction = typing.rules.expression.IdentifierCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.IdentifierCombineFunctionDeltatype(res._2)
        typing.rules.expression.IdentifierDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "n" => {
        val hyperCombinationFunction = typing.rules.expression.NumericalCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.NumericalCombineFunctionDeltatype(res._2)
        typing.rules.expression.NumericalDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "b" => {
        val hyperCombinationFunction = typing.rules.expression.BooleanCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.BooleanCombineFunctionDeltatype(res._2)
        typing.rules.expression.BooleanDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "negate" => {
        val hyperCombinationFunction = typing.rules.expression.unaryOp.NegateCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.unaryOp.NegateCombineFunctionDeltatype(res._2)
        typing.rules.expression.unaryOp.NegateDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "methodCall" => {
        val hyperCombinationFunction = typing.rules.expression.MethodCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.MethodCombineFunctionDeltatype(res._2)
        typing.rules.expression.MethodDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "lookup" => {
        val hyperCombinationFunction = typing.rules.expression.LookupCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.LookupCombineFunctionDeltatype(res._2)
        typing.rules.expression.LookupDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "length" => {
        val hyperCombinationFunction = typing.rules.expression.LengthCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.LengthCombineFunctionDeltatype(res._2)
        typing.rules.expression.LengthDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "&&" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.AndCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.AndCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.AndDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "||" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.OrCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.OrCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.OrDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "setminus" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.SetminusCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.SetminusCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.SetminusDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "union" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.UnionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.UnionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.UnionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "in" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.InCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.InCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.InDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "==" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.EqualityCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.EqualityCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.EqualityDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "!=" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.InequalityCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.InequalityCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.InequalityDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case ">" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.GreaterCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.GreaterCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.GreaterDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case ">=" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.GreaterEqualCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.GreaterEqualCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.GreaterEqualDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "<" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.SmallerCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.SmallerCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.SmallerDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "<=" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.SmallerEqualCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.SmallerEqualCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.SmallerEqualDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "/" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.DivisionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.DivisionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.DivisionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "++" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.ExtensionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.ExtensionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.ExtensionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "==>" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.ImplicationCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.ImplicationCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.ImplicationDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "%" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.ModuloCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.ModuloCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.ModuloDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "*" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.MultiplicationCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.MultiplicationCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.MultiplicationDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "-" => {
        val hyperCombinationFunction = typing.rules.expression.binaryOp.SubtractionCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.binaryOp.SubtractionCombineFunctionDeltatype(res._2)
        typing.rules.expression.binaryOp.SubtractionDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case "!" => {
        val hyperCombinationFunction = typing.rules.expression.unaryOp.NotCombineFunctionHypertype(res._1)
        val deltaCombinationFunction = typing.rules.expression.unaryOp.NotCombineFunctionDeltatype(res._2)
        typing.rules.expression.unaryOp.NotDerivationRule(hyperCombinationFunction, deltaCombinationFunction)
      }
      case _ => {
        throw new Exception(s"Unsupported operator: $op")
      }
    }
  }
}

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion]) {
  def toTypeSystem(arity: Int): (Seq[typing.rules.RuleWrapper[typing.rules.HyperTypeConclusion]], Seq[typing.rules.RuleWrapper[typing.rules.DeltaConclusion]]) = {
    val variables   = conditions.flatMap(_.variables).toSet ++ conclusions.flatMap(_.variables).toSet
    val variableMap = variables.zipWithIndex.toMap

    val hyperTypeConclusions = conclusions.filter(conclusion => conclusion.isHyperTypeConclusion()).map(_.toTypingCondition(variableMap).map(_.asInstanceOf[typing.rules.HyperTypeConclusion]))
    val deltaConclusions     = conclusions.filter(conclusion => !conclusion.isHyperTypeConclusion()).map(_.toTypingCondition(variableMap).map(_.asInstanceOf[typing.rules.DeltaConclusion]))

    val typeingConditions = conditions.flatMap(cond => cond.toTypingCondition(variableMap))
    val neededLength      = arity * 2 + 1
    val aggregator        = Array.ofDim[Seq[typing.rules.Condition]](neededLength + 1)
    for (i <- 0 to neededLength) {
      aggregator(i) = Seq.empty[typing.rules.Condition]
    }
    val orderedConditions = typeingConditions.foldLeft(aggregator) { (acc, current) =>
      val index        = current._1
      val condition    = current._2
      val existingList = acc(index)
      if (existingList == null) {
        acc(index) = Seq(condition)
      } else {
        acc(index) = existingList :+ condition
      }
      acc
    }

    val numVariables   = variableMap.size
    val hyperTypeRules = hyperTypeConclusions.map { conclusion =>
      {
        val combiningFunction = arity match {
          case 0 =>
            typing.rules.nullaryFunctionImplication[typing.rules.HyperTypeConclusion](
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case 1 =>
            typing.rules.unaryFunctionImplication[typing.rules.HyperTypeConclusion](
              orderedConditions(1).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(2).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case 2 =>
            typing.rules.binaryFunctionImplication[typing.rules.HyperTypeConclusion](
              orderedConditions(1).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(2).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions(3).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(4).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case _ => throw new Exception(s"Unsupported arity: ${arity}. 0-2 are supported.")
        }
        if (numVariables == 0) {
          typing.rules.EmptyWrapper(
            combiningFunction
          )
        } else if (numVariables == 1) {
          typing.rules.ForanyVariableWrapper(
            combiningFunction
          )
        } else {
          throw new Exception("Unsupported number of variables: " + numVariables)
        }
      }
    }
    val deltaTypeRules = deltaConclusions.map { conclusion =>
      {
        val combiningFunction = arity match {
          case 0 =>
            typing.rules.nullaryFunctionImplication[typing.rules.DeltaConclusion](
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case 1 =>
            typing.rules.unaryFunctionImplication[typing.rules.DeltaConclusion](
              orderedConditions(1).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(2).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case 2 =>
            typing.rules.binaryFunctionImplication[typing.rules.DeltaConclusion](
              orderedConditions(1).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(2).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions(3).map(_.asInstanceOf[typing.rules.HyperTypeCondition]),
              orderedConditions(4).map(_.asInstanceOf[typing.rules.DeltaCondition]),
              orderedConditions.head.map(_.asInstanceOf[typing.rules.SideCondition]),
              conclusion
            )
          case _ => throw new Exception(s"Unsupported arity: ${arity}. 0-2 are supported.")
        }
        if (numVariables == 0) {
          typing.rules.EmptyWrapper(
            combiningFunction
          )
        } else if (numVariables == 1) {
          typing.rules.ForanyVariableWrapper(
            combiningFunction
          )
        } else {
          throw new Exception("Unsupported number of variables: " + numVariables)
        }
      }
    }

    (hyperTypeRules, deltaTypeRules) // TODO: Implement conversion to TypeSystem rules
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
  def toTypingCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)]

}

case class InSet(elem: Element, set: Set) extends Condition {

  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)] = {
    set match {
      case _ @HyperCollection(id) => {
        val hyperType = elem.toIndexed(variableMap).asInstanceOf[HyperType]
        val index     = id.intValue() * 2 + 1
        Seq((index, typing.rules.ElementOf(hyperType)))
      }
      case _ @HyperCollectionResult()    => throw new Exception("HyperCollectionResult cannot be used in InSet condition")
      case _ @MappingAccess(mapping, id) => {
        val hyperType = elem.asInstanceOf[HyperType]
        mapping match {
          case DeltaCollectionResult() => throw new Exception("DeltaCollectionResult cannot be used in InSet condition")
          case DeltaCollection(setId)  => {
            val index    = setId.intValue() * 2 + 1
            val varIndex = variableMap.get(id).getOrElse(throw new Exception("Variable " + id.name + " not found in variable map"))
            Seq((index, typing.rules.DeltaContains(varIndex, hyperType)))
          }
          case Delta() => {
            val index    = 0
            val varIndex = variableMap.get(id).getOrElse(throw new Exception("Variable " + id.name + " not found in variable map"))
            Seq((index, typing.rules.DeltaContains(varIndex, hyperType)))
          }
          case Gamma() => throw new Exception("Not implemented yet")
          case _       => throw new Exception("Unsupported mapping type for InSet condition")
        }
      }
      case _ => throw new Exception("Unsupported set type for InSet condition")
    }
  }

}

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
  def toTypingCondition(variableMap: Map[Id, Int]): Seq[typing.rules.Conclusion]
}
case class AddToSet(elem: Element, set: Set) extends Conclusion {

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[typing.rules.Conclusion] = {
    set match {
      case HyperCollectionResult() => {
        val hyperType = elem.toIndexed(variableMap).asInstanceOf[HyperType]
        Seq(typing.rules.ContainsHyperType(hyperType))
      }
      case MappingAccess(mapping, id) => {
        mapping match {
          case Delta() => {
            val hyperType = elem.toIndexed(variableMap).asInstanceOf[HyperType]
            val varIndex  = variableMap.get(id).getOrElse(throw new Exception("Variable " + id.name + " not found in variable map"))
            Seq(typing.rules.VarHasDeltaType(varIndex, hyperType))
          }
          case _ => throw new Exception("Unsupported mapping type for AddToSet conclusion")
        }
      }
      case _ => throw new Exception("Unsupported Set type for AddToSet conclusion")
    }
  }

  override def variables: immutable.Set[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}
case class SetEquals(set1: Set, set2: Set) extends Conclusion {

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[typing.rules.Conclusion] = {
    (set1, set2) match {
      case (MappingAccess(Gamma(), varId), HyperCollectionResult()) => {
        val variableIndex = variableMap.get(varId).getOrElse(throw new Exception("Variable " + varId.name + " not found in variable map"))
        Seq(typing.rules.LookupAndAddHyperType(variableIndex))
      }
      case _ => throw new Exception("Unsupported Set types for SetEquals conclusion")
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
