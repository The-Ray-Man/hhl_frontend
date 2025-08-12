package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
  def apply(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext, result: ExpressionDerivationResult): ExpressionDerivationResult
}

case class AddToSet(elem: Element, set: Set) extends Conclusion {

  def apply(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext, result: ExpressionDerivationResult): ExpressionDerivationResult = {
    set match {
      case HyperCollectionResult() => {
        val updatedElem = applyIndexed.applyIndexed(ruleCheckContext.variables, elem).asInstanceOf[HyperType]
        ExpressionDerivationResult(
          hyperTypeCollection = result.hyperTypeCollection.add(updatedElem),
          deltaCollection = result.deltaCollection
        )
      }
      case _: Set => throw new Exception("Not implemented yet")
    }
  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}

case class SetEquals(set1: Set, set2: Set) extends Conclusion {

  def hyperTypeResultToGammaLookup(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext, result: ExpressionDerivationResult, index: Id): ExpressionDerivationResult = {
    context.varMapping.getOrElse(index, throw new Exception(s"Variable $index not found in variable mapping")) match {
      case Id(name) => {
        val hyperTypes = context.gamma.getUnsafe(name)
        ExpressionDerivationResult(
          hyperTypeCollection = hyperTypes,
          deltaCollection = result.deltaCollection
        )
      }
      case _ => throw new Exception("Expected Id for index in MappingAccess")
    }
  }

  override def apply(context: ExpressionDerivationContext, ruleCheckContext: RuleCheckContext, result: ExpressionDerivationResult): ExpressionDerivationResult = {
    (set1, set2) match {
      case (HyperCollectionResult(), MappingAccess(Gamma(), index)) => hyperTypeResultToGammaLookup(context, ruleCheckContext, result, index)
      case (MappingAccess(Gamma(), index), HyperCollectionResult()) => hyperTypeResultToGammaLookup(context, ruleCheckContext, result, index)
      case _                                                        => throw new Exception("Not yet implemented")
    }
  }

  override def variables: ScalaSet[Id] = set1.variables ++ set2.variables

  override def isHyperTypeConclusion(): Boolean = {
    val res1 = set1.isHyperTypeConclusion()
    val res2 = set2.isHyperTypeConclusion()
    if (res1 != res2) {
      throw new Exception("SetEquals conclusion must have both sets of the same type")
    }
    res1
  }

}
