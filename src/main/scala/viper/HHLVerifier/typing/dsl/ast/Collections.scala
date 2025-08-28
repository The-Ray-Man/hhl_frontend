package viper.HHLVerifier.typing.dsl.ast

import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.dsl.CollectVariables
import viper.HHLVerifier.typing.dsl.Element
import viper.HHLVerifier.ast.Id

// Building Blocks for Condition and Conclusion

trait Mapping extends CollectVariables
trait Set     extends CollectVariables

case class WithoutElement(set: Set, elem: Element) extends Set {
  override def variables: ScalaSet[Id] = set.variables ++ elem.variables
}

case class AssignedVariables(stmt: Id) extends Set {
  override def variables: ScalaSet[Id] = ScalaSet(stmt)
}

case class Variables(content: Id) extends Set {

  override def variables: ScalaSet[Id] = ScalaSet(content)
}

case class AllVariables() extends Set {
  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}

case class AllParameters() extends Set {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]
}

case class HyperTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Set {

  override def variables: ScalaSet[Id] = ScalaSet(expr) ++ gamma.variables ++ delta.variables

}

case class DeriveHyperType(expr: Id, gamma: Mapping, delta: Mapping) extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet(expr) ++ gamma.variables ++ delta.variables

}

case class DeriveDeltaType(expr: Id, gamma: Mapping, delta: Mapping) extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet(expr) ++ gamma.variables ++ delta.variables

}

case class InitializeDeltaMapping() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty
}

case class InitializeGammaMapping() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty
}

case class DeltaTypeCheck(expr: Id, gamma: Mapping, delta: Mapping) extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet(expr) ++ gamma.variables ++ delta.variables
}

case class HyperCollectionResult() extends Set {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]
}
case class MappingAccess(mapping: Mapping, id: Id) extends Set with Mapping {

  override def variables: ScalaSet[Id] = ScalaSet(id) ++ mapping.variables
}

case class DeltaCollectionResult() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}
case class Gamma() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}

case class Delta() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}

case class GammaResult() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}

case class DeltaResult() extends Mapping {

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]

}
