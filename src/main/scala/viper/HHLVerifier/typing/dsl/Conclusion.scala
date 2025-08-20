package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.dsl.Parser.set
import viper.HHLVerifier.typing.dsl.Parser.mapping

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
  def apply(context: Context, result: DerivationResult): DerivationResult
}

case class AddToSet(elem: Element, set: Set) extends Conclusion {

  def apply(context: Context, result: DerivationResult): DerivationResult = {
    set match {
      case HyperCollectionResult() => {
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection.add(elem.asInstanceOf[HyperType]),
          deltaCollection = result.getExpressionResult.deltaCollection
        )
      }
      case MappingAccess(DeltaCollectionResult(), variable) => {
        val variableLookup = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection,
          deltaCollection = result.getExpressionResult.deltaCollection.add(variableLookup.name, elem.asInstanceOf[HyperType])
        )
      }
      case MappingAccess(GammaResult(), variable) => {
        val variableLookup         = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val hyperTypes             = result.getStatementResult.hyperTypeMapping.get(variableLookup.name)
        val newHyperTypeCollection = hyperTypes.add(elem.asInstanceOf[HyperType])
        StatementDerivationResult(
          hyperTypeMapping = HyperMapping(result.getStatementResult.hyperTypeMapping.mapping.updated(variableLookup.name, newHyperTypeCollection)),
          deltaMapping = result.getStatementResult.deltaMapping
        )
      }
      case _: Set => throw new Exception("Not implemented yet: " + set.getClass.getSimpleName)
    }
  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}

case class SetEquals(set1: Set, set2: Set) extends Conclusion with Condition {

  override def check(context: Context, expression: Boolean): Boolean = {
    val utils  = DeriveArgsUtils(context)
    val setLhs = utils.getHyperTypeCollection(set1)
    val setRhs = utils.getHyperTypeCollection(set2)
    setLhs == setRhs
  }

  def hyperTypeResultToGammaLookup(context: Context, result: DerivationResult, index: Id): DerivationResult = {
    context.getExprById(index) match {
      case Id(name) => {
        val hyperTypes = context.gamma.get(name)
        ExpressionDerivationResult(
          hyperTypeCollection = hyperTypes,
          deltaCollection = result.getExpressionResult.deltaCollection
        )
      }
      case _ => throw new Exception("Expected Id for index in MappingAccess")
    }
  }

  def setHyperMapping(context: Context, variable: Id, collection: Set, result: DerivationResult): DerivationResult = {
    val hyperTypeCollection = DeriveArgsUtils(context).getHyperTypeCollection(collection)
    val indexedVariable     = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
    val updatedMapping      = result.getStatementResult.hyperTypeMapping.set(indexedVariable.name, hyperTypeCollection)
    StatementDerivationResult(
      hyperTypeMapping = updatedMapping,
      deltaMapping = context.delta
    )
  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (set1, set2) match {
      case (HyperCollectionResult(), MappingAccess(Gamma(), index)) => hyperTypeResultToGammaLookup(context, result, index)
      case (MappingAccess(Gamma(), index), HyperCollectionResult()) => hyperTypeResultToGammaLookup(context, result, index)
      case (MappingAccess(GammaResult(), index), _)                 => setHyperMapping(context, index, set2, result)
      case (_, MappingAccess(GammaResult(), index))                 => setHyperMapping(context, index, set1, result)
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

case class MapEquals(mapping1: Mapping, mapping2: Mapping) extends Conclusion with Condition {

  val mapping1Type = findMappingType(mapping1)
  val mapping2Type = findMappingType(mapping2)

  def findMappingType(mapping: Mapping): String = {
    mapping match {
      case DeriveHyperType(_, _, _, _) | Gamma() | GammaResult()                                                                         => "HyperMapping"
      case DeltaTypeCheck(_, _, _) | MappingAccess(DeriveDeltaType(_, _, _, _), _) | MappingAccess(Delta(), _) | DeltaCollectionResult() => "DeltaCollection"
      case DeriveDeltaType(_, _, _, _) | Delta() | DeltaResult()                                                                         => "DeltaMapping"
    }
  }

  override def check(context: Context, expression: Boolean): Boolean = {
    if (mapping1Type != mapping2Type) {
      return false
    }
    val utils = DeriveArgsUtils(context)
    mapping1Type match {
      case "HyperMapping"    => utils.getHyperMapping(mapping1) == utils.getHyperMapping(mapping2)
      case "DeltaCollection" => utils.getDeltaCollection(mapping1) == utils.getDeltaCollection(mapping2)
      case "DeltaMapping"    => utils.getDeltaMapping(mapping1) == utils.getDeltaMapping(mapping2)
    }
  }

  def gammaResultEqualsDeriveHyperType(context: Context, deriveHyperType: DeriveHyperType, result: DerivationResult): DerivationResult = {
    val derivedHyperMapping = DeriveArgsUtils(context).getHyperMapping(deriveHyperType)
    StatementDerivationResult(
      hyperTypeMapping = derivedHyperMapping,
      deltaMapping = result.getStatementResult.deltaMapping
    )
  }

  def gammaResultEqualsDeriveDeltaType(context: Context, deriveDeltaType: DeriveDeltaType, result: DerivationResult): DerivationResult = {
    val derivedDeltaMapping = DeriveArgsUtils(context).getDeltaMapping(deriveDeltaType)
    StatementDerivationResult(
      hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
      deltaMapping = derivedDeltaMapping
    )
  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (mapping1, mapping2) match {
      case (GammaResult(), d: DeriveHyperType) => gammaResultEqualsDeriveHyperType(context, d, result)
      case (d: DeriveHyperType, GammaResult()) => gammaResultEqualsDeriveHyperType(context, d, result)
      case (DeltaResult(), d: DeriveDeltaType) => gammaResultEqualsDeriveDeltaType(context, d, result)
      case (d: DeriveDeltaType, DeltaResult()) => gammaResultEqualsDeriveDeltaType(context, d, result)
    }
  }

  override def variables: ScalaSet[Id] = mapping1.variables ++ mapping2.variables

  override def isHyperTypeConclusion(): Boolean = mapping1.isHyperTypeConclusion() && mapping2.isHyperTypeConclusion()

}
