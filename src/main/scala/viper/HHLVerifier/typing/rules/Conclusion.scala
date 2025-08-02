package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.HyperType
import viper.HHLVerifier.ast.Id


trait Conclusion {
    def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection)
}


trait HyperTypeConclusion extends Conclusion {}

trait DeltaConclusion extends Conclusion {}


case class ContainsHyperType(val hyperType: HyperType) extends HyperTypeConclusion {

  override def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection) = {
    (hyperTypeCollection.add(hyperType), deltaCollection)
  }

}

case class VarHasDeltaType(val variable: Id, val hyperType: HyperType) extends DeltaConclusion {

  override def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection) = {
    (hyperTypeCollection, deltaCollection.add(variable.name, hyperType))
  }

}

case class LookupAndAddHyperType() extends HyperTypeConclusion {

  override def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection, mapping: HyperMapping, context: Any): (HyperTypeCollection, DeltaCollection) = {
    val varName = context match {
      case id: Id => id.name
      case _ => throw new Exception("Expected Id, got: " + context)
    }
    
    (hyperTypeCollection.extend(mapping.getUnsafe(varName)), deltaCollection)
  }

}