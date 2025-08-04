package viper.HHLVerifier.typing.dsl

case class Specification(derivationRules: Seq[DerivationRule]) {}

trait DerivationRule

case class ExpressionDerivationRule(op: String, inputs : Seq[(HyperCollection, DeltaCollection)], rules : Seq[Rule]) extends DerivationRule

case class Rule(conditions: Seq[Condition], conclusions: Seq[Conclusion])


// Building Blocks for Condition and Conclusion
trait Mapping {} 
trait Set {}


case class HyperCollection(id: Number) extends Set
case class DeltaCollection(id: Number) extends Mapping
case class HyperCollectionResult() extends Set
case class DeltaCollectionResult() extends Mapping
case class Gamma() extends Mapping
case class Delta() extends Mapping
case class MappingAccess(mapping: Mapping, id: String) extends Set

case class Element(name: String)




// Building Blocks for Conditions/Conclusion
trait Condition {}

case class InSet(elem: Element, set: Set) extends Condition

trait Conclusion {}
case class AddToSet(elem: Element, set: Set) extends Conclusion
case class SetEquals(set1: Set, set2: Set) extends Conclusion