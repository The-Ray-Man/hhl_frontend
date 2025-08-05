package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperMapping

trait Action {
  def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection)
}

case class AddHyperType(hyperType: HyperType) extends Action {
  def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection) = {
    (hyperTypeCollection.add(hyperType), deltaCollection)
  }
}

case class ExtendDeltaMapping(variable: Id, hyperType: HyperType) extends Action {
  def apply(hyperTypeCollection: HyperTypeCollection, deltaCollection: DeltaCollection): (HyperTypeCollection, DeltaCollection) = {
    (hyperTypeCollection, deltaCollection.add(variable.name, hyperType))
  }
}

trait Conclusion {
  def toActions(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action]
}

trait HyperTypeConclusion extends Conclusion {}

trait DeltaConclusion extends Conclusion {}

case class ContainsHyperType(val hyperType: HyperType) extends HyperTypeConclusion {

  override def toActions(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = {
    Seq(AddHyperType(hyperType))
  }
}

case class VarHasDeltaType(val varId: Int, val hyperType: HyperType) extends DeltaConclusion {

  override def toActions(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = {
    if (ruleCheckContext.variables.length <= varId) {
      throw new Exception("Variable index out of bounds: " + varId + " for variables: " + ruleCheckContext.variables)
    }

    val variable = ruleCheckContext.variables(varId)
    Seq(ExtendDeltaMapping(variable, hyperType))
  }
}

case class LookupAndAddHyperType(val varId: Int) extends HyperTypeConclusion {

  override def toActions(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext): Seq[Action] = {
    val hyperTypes = context.mapping.getUnsafe(ruleCheckContext.variables(varId).name)
    val result     = hyperTypes.hypertypes.map(ht => AddHyperType(ht)).toSeq

    result

  }

}
