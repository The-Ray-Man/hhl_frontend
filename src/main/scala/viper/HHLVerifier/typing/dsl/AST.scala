package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.rules.TypeSystem
import viper.HHLVerifier.typing
import scala.collection.immutable
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.rules.expression._
import viper.HHLVerifier.typing.rules.expression.binaryOp._
import viper.HHLVerifier.typing.rules.expression.unaryOp._
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.AssertVarDecl
import viper.HHLVerifier.ast.Assertion
import viper.HHLVerifier.ast.SetAssignExpr
import viper.HHLVerifier.ast.SeqAssignExpr
import viper.HHLVerifier.ast.HintDecl
import viper.HHLVerifier.ast.MapTupleExpr
import viper.HHLVerifier.ast.SpecialId
import viper.HHLVerifier.ast.LoopIndex
import viper.HHLVerifier.ast.StateExistsExpr
import viper.HHLVerifier.ast.MapAssignExpr
import viper.HHLVerifier.ast.Hint
import viper.HHLVerifier.ast.CombExpr
import viper.HHLVerifier.ast.UpdateMapExpr

case class Specification(hypertypeDeclaration: Seq[HyperTypeDeclaration],derivationRules: Seq[DerivationRule]) {

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
  
  def isApplicableTo(toCheckExpression: Expr) : Option[Map[Id, Expr]] = {
    // Check if the expression matches the rule's expression
    // Returns a mapping that maps the "template ids" to the Id in the expression.
    (expr, toCheckExpression) match {
      case (BinaryExpr(left, op, right), BinaryExpr(leftCheck, opCheck, rightCheck)) if op == opCheck => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (UnaryExpr(op, inner), UnaryExpr(opCheck, innerCheck)) if op == opCheck => Some(matchHelper(Seq((inner, innerCheck))))
      case (Id("n"), Num(value)) => Some(Map(Id("n") -> Num(value)))
      case (Id("var"), Id(name)) => Some(Map(Id("var") -> Id(name)))
      case (Id("b"), BoolLit(name)) => Some(Map(Id("b") -> BoolLit(name)))
      case (ImpliesExpr(left, right), ImpliesExpr(leftCheck, rightCheck)) => Some(matchHelper(Seq((left, leftCheck), (right, rightCheck))))
      case (_, MethodCallExpr(_,_)) => throw new Exception("Method calls are not supported in expression derivation rules")
      case (LookupExpr(dataStructure, index), LookupExpr(dataStructureCheck, indexCheck)) => Some(matchHelper(Seq((dataStructure, dataStructureCheck), (index, indexCheck))))
      case (LengthExpr(dataStructure), LengthExpr(dataStructureCheck)) => Some(matchHelper(Seq((dataStructure, dataStructureCheck))))
      case _ => None
    }
  }

  def matchHelper(matching: Seq[(Expr, Expr)]) : Map[Id, Expr] = {
    matching.foldLeft(Map.empty[Id, Expr]) { (acc, pair) =>
      (pair._1, pair._2) match {
        case (id: Id, expr: Expr) => acc + (id -> expr)
        case _ => throw new Exception("The template expression can not be recursive")
      }
    }
  }

  def derive(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, expr: Expr, variableMapping: Map[Id, Expr]) : (typing.HyperTypeCollection, typing.DeltaCollection) = {
    val (hyperTypes, deltaTypes) = (typing.HyperTypeCollection.fromSeq(Seq.empty), typing.DeltaCollection(mapping = Map.empty))
    rules.foreach(rule => {
      val condition = rule.conditions.forall(cond => cond.check(typeSystem, gamma, delta, variableMapping))
      if (condition) {
        // rule.conclusions.foreach(conclusion => {
        //   conclusion.
        // })
      }
    })



    (hyperTypes, deltaTypes) 
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

  def check(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, mapping: Map[Id, Expr]): Boolean
}

case class InSet(elem: Element, set: Set) extends Condition {

  override def check(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, mapping: Map[Id,Expr]): Boolean = {
    val indexedSet = set match {
      case HyperTypeCheck(id, _, _) => typeSystem.deriveExpression(gamma, delta, id, mapping)._1 // Todo gamma, delta collection should be infered.
      case _: Set => throw new Exception("Not implemented yet")
    }

    indexedSet.hypertypes.contains(elem.asInstanceOf[HyperType])
  }


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

case class ArithCondition(variable: Id, op: String, right: Int) extends Condition {

  override def check(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, mapping: Map[Id,Expr]): Boolean = {
    val expression = mapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
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

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)] = {
    Seq((0, typing.rules.ArithCondition(op, right)))
  }
}

case class BoolCondition(variable: Id) extends Condition {

  override def check(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, mapping: Map[Id,Expr]): Boolean = {
    val expression = mapping.get(variable).getOrElse(throw new Exception("Variable " + variable.name + " not found in mapping"))
    expression match {
      case BoolLit(value) => value
      case _              => throw new Exception("BoolCondition can only be checked against BoolLit expressions")
    }
  }


  override def variables: immutable.Set[Id] = Set(variable)

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)] = {
    Seq((0, typing.rules.BoolCondition()))
  }
}


case class NotOperator(condition: Condition) extends Condition {

  override def check(typeSystem: TypeSystem, gamma: typing.HyperMapping, delta: typing.DeltaMapping, mapping: Map[Id,Expr]): Boolean = {
    val subConditionResult = condition.check(typeSystem, gamma, delta, mapping)
    !subConditionResult
  }


  override def variables: immutable.Set[Id] = condition.variables

  override def toTypingCondition(variableMap: Map[Id, Int]): Seq[(Int, typing.rules.Condition)] = {
    val subCondition = condition.toTypingCondition(variableMap)
    subCondition.map { case (index, cond) =>
      cond match {
        case _: typing.rules.SideCondition => (index, typing.rules.NegateSideCondition(cond.asInstanceOf[typing.rules.SideCondition]))
        case _: typing.rules.HyperTypeCondition => (index, typing.rules.NegateHyperCondition(cond.asInstanceOf[typing.rules.HyperTypeCondition]))
        case _: typing.rules.DeltaCondition => (index, typing.rules.NegateDeltaCondition(cond.asInstanceOf[typing.rules.DeltaCondition]))
        case _ => throw new Exception("Unsupported condition type for NotOperator")
      }
    }
  }
}
