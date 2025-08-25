package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.dsl.Parser.set
import viper.HHLVerifier.typing.dsl.Parser.mapping
import viper.HHLVerifier.typing.DeltaCollection

trait Conclusion extends CollectVariables with ConclusionInfo {
  def isHyperTypeConclusion(): Boolean
  def apply(context: Context, result: DerivationResult): DerivationResult
}

case class AddToSet(elem: Element, set: Set) extends Conclusion {

  def apply(context: Context, result: DerivationResult): DerivationResult = {
    val elementIndexed = context.applyIndexed(elem)
    set match {
      case HyperCollectionResult() => {
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection.add(elementIndexed.asInstanceOf[HyperType]),
          deltaCollection = result.getExpressionResult.deltaCollection
        )
      }
      case MappingAccess(DeltaCollectionResult(), variable) => {
        val variableLookup     = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val newDeltaCollection = result.getExpressionResult.deltaCollection.add(variableLookup.name, elementIndexed.asInstanceOf[HyperType])
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection,
          deltaCollection = newDeltaCollection
        )
      }
      case MappingAccess(GammaResult(), variable) => {
        val variableLookup         = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val hyperTypes             = result.getStatementResult.hyperTypeMapping.get(variableLookup.name)
        val newHyperTypeCollection = hyperTypes.add(elementIndexed.asInstanceOf[HyperType])
        StatementDerivationResult(
          hyperTypeMapping = HyperMapping(result.getStatementResult.hyperTypeMapping.mapping.updated(variableLookup.name, newHyperTypeCollection)),
          deltaMapping = result.getStatementResult.deltaMapping
        )
      }
      case MappingAccess(MappingAccess(DeltaResult(), var1), var2) => {
        val variableLookup1        = context.varExprMapping.getOrElse(var1, var1).asInstanceOf[Id]
        val variableLookup2        = context.varExprMapping.getOrElse(var2, var2).asInstanceOf[Id]
        val htype                  = elementIndexed.asInstanceOf[HyperType]
        val deltaMapping           = result.getStatementResult.deltaMapping
        val deltaCollection        = deltaMapping.collection.get(variableLookup2.name)
        val updatedDeltaCollection = deltaCollection match {
          case Some(dc) => dc.add(variableLookup1.name, htype)
          case None     => DeltaCollection(Map(variableLookup1.name -> HyperTypeCollection(ScalaSet(htype))))
        }
        val updatedDeltaMapping = DeltaMapping(deltaMapping.collection.updated(variableLookup2.name, updatedDeltaCollection))
        StatementDerivationResult(
          hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
          deltaMapping = updatedDeltaMapping
        )
      }
      case _: Set => throw new Exception("Not implemented yet: " + set.getClass.getSimpleName)
    }
  }
  override def variables: ScalaSet[Id] = elem.variables ++ set.variables

  override def isHyperTypeConclusion(): Boolean = set.isHyperTypeConclusion()

}

case class ExtendSet(toAdd: Set, toExtend: Set) extends Conclusion {

  override def variables: ScalaSet[Id] = toAdd.variables ++ toExtend.variables

  override def isHyperTypeConclusion(): Boolean = toAdd.isHyperTypeConclusion() && toExtend.isHyperTypeConclusion()

  def apply(context: Context, result: DerivationResult): DerivationResult = {
    val utils = DeriveArgsUtils(context)
    toExtend match {
      case HyperCollectionResult() => {
        val toAddSet = utils.getHyperTypeCollection(toAdd)
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection.extend(toAddSet),
          deltaCollection = result.getExpressionResult.deltaCollection
        )
      }
      case MappingAccess(DeltaCollectionResult(), variable) => {
        val variableLookup = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val toAddSet       = utils.getHyperTypeCollection(toAdd)
        ExpressionDerivationResult(
          hyperTypeCollection = result.getExpressionResult.hyperTypeCollection,
          deltaCollection = result.getExpressionResult.deltaCollection.extend(variableLookup.name, toAddSet)
        )
      }
      case MappingAccess(GammaResult(), variable) => {
        val variableLookup         = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val hyperTypes             = result.getStatementResult.hyperTypeMapping.get(variableLookup.name)
        val toAddSet               = utils.getHyperTypeCollection(toAdd)
        val newHyperTypeCollection = hyperTypes.extend(toAddSet)
        StatementDerivationResult(
          hyperTypeMapping = HyperMapping(result.getStatementResult.hyperTypeMapping.mapping.updated(variableLookup.name, newHyperTypeCollection)),
          deltaMapping = result.getStatementResult.deltaMapping
        )
      }
    }
  }
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

  def setDeltaCollection(context: Context, var1: Id, var2: Id, other: Set, result: DerivationResult): DerivationResult = {
    val indexedVar1     = context.applyIndexed(var1).asInstanceOf[Id]
    val indexedVar2     = context.applyIndexed(var2).asInstanceOf[Id]
    val deltaCollection = DeriveArgsUtils(context).getDeltaTypes(other)
    val delta           = context.delta
    delta.collection.get(indexedVar2.name) match {
      case Some(dc) => {
        val updatedDeltaCollection = dc.mapping.updated(indexedVar1.name, deltaCollection)
        val updatedDeltaMapping    = DeltaMapping(delta.collection.updated(indexedVar2.name, DeltaCollection(updatedDeltaCollection)))
        StatementDerivationResult(
          hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
          deltaMapping = updatedDeltaMapping
        )
      }
      case None => {
        val updatedDeltaMapping = DeltaMapping(delta.collection.updated(indexedVar2.name, DeltaCollection(Map(indexedVar1.name -> deltaCollection))))
        StatementDerivationResult(
          hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
          deltaMapping = updatedDeltaMapping
        )
      }
    }
  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (set1, set2) match {
      case (HyperCollectionResult(), MappingAccess(Gamma(), index))         => hyperTypeResultToGammaLookup(context, result, index)
      case (MappingAccess(Gamma(), index), HyperCollectionResult())         => hyperTypeResultToGammaLookup(context, result, index)
      case (MappingAccess(GammaResult(), index), _)                         => setHyperMapping(context, index, set2, result)
      case (_, MappingAccess(GammaResult(), index))                         => setHyperMapping(context, index, set1, result)
      case (MappingAccess(MappingAccess(DeltaResult(), var1), var2), other) => setDeltaCollection(context, var1, var2, other, result)
      case (other, MappingAccess(MappingAccess(DeltaResult(), var1), var2)) => setDeltaCollection(context, var1, var2, other, result)

      case _ => throw new Exception("Not yet implemented")
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
      case DeriveHyperType(_, _, _, _) | Gamma() | GammaResult()                                                                                                           => "HyperMapping"
      case DeltaTypeCheck(_, _, _) | MappingAccess(DeriveDeltaType(_, _, _, _), _) | MappingAccess(Delta(), _) | DeltaCollectionResult() | MappingAccess(DeltaResult(), _) => "DeltaCollection"
      case DeriveDeltaType(_, _, _, _) | Delta() | DeltaResult()                                                                                                           => "DeltaMapping"
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

  def deltaResultEqualsDeriveDeltaType(context: Context, deriveDeltaType: DeriveDeltaType, result: DerivationResult): DerivationResult = {
    val derivedDeltaMapping = DeriveArgsUtils(context).getDeltaMapping(deriveDeltaType)
    StatementDerivationResult(
      hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
      deltaMapping = derivedDeltaMapping
    )
  }

  def deltaResultEqualsDeltaLookup(context: Context, variable: Id, result: DerivationResult): DerivationResult = {
    val indexedVariable = context.applyIndexed(variable).asInstanceOf[Id]
    val deltaType       = context.delta.collection.getOrElse(indexedVariable.name, throw new Exception("Variable " + indexedVariable.name + " not found in Delta mapping"))
    ExpressionDerivationResult(
      hyperTypeCollection = result.getExpressionResult.hyperTypeCollection,
      deltaCollection = deltaType
    )
  }

  def deltaResultEqualsDeltaLookup(context: Context, resultVar: Id, lookupVar: Id, result: DerivationResult): DerivationResult = {
    val indexedResultVar = context.applyIndexed(resultVar).asInstanceOf[Id]
    val indexedLookupVar = context.applyIndexed(lookupVar).asInstanceOf[Id]
    val deltaCollection  = context.delta.collection.getOrElse(indexedLookupVar.name, throw new Exception("Variable " + indexedLookupVar.name + " not found in Delta mapping"))
    val newDeltaMapping  = context.delta.collection.updated(indexedResultVar.name, deltaCollection)

    StatementDerivationResult(
      hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
      deltaMapping = DeltaMapping(newDeltaMapping)
    )
  }

  def deltaResultEqualsDeltaTypeCheck(context: Context, variable: Id, deltaTypeCheck: DeltaTypeCheck, result: DerivationResult): DerivationResult = {
    val indexedVariable = context.applyIndexed(variable).asInstanceOf[Id]
    val deltaMapping    = DeriveArgsUtils(context).getDeltaCollection(deltaTypeCheck)
    val newDeltaMapping = context.delta.collection.updated(indexedVariable.name, deltaMapping)
    StatementDerivationResult(
      hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
      deltaMapping = DeltaMapping(newDeltaMapping)
    )

  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (mapping1, mapping2) match {
      case (GammaResult(), d: DeriveHyperType) => gammaResultEqualsDeriveHyperType(context, d, result)
      case (d: DeriveHyperType, GammaResult()) => gammaResultEqualsDeriveHyperType(context, d, result)

      case (DeltaResult(), d: DeriveDeltaType) => deltaResultEqualsDeriveDeltaType(context, d, result)
      case (d: DeriveDeltaType, DeltaResult()) => deltaResultEqualsDeriveDeltaType(context, d, result)

      case (DeltaCollectionResult(), MappingAccess(Delta(), variable)) => deltaResultEqualsDeltaLookup(context, variable, result)
      case (MappingAccess(Delta(), variable), DeltaCollectionResult()) => deltaResultEqualsDeltaLookup(context, variable, result)

      case (deltacheck: DeltaTypeCheck, MappingAccess(DeltaResult(), variable)) => deltaResultEqualsDeltaTypeCheck(context, variable, deltacheck, result)
      case (MappingAccess(DeltaResult(), variable), deltacheck: DeltaTypeCheck) => deltaResultEqualsDeltaTypeCheck(context, variable, deltacheck, result)

      case (MappingAccess(DeltaResult(), resultVar), MappingAccess(Delta(), lookupVar)) => deltaResultEqualsDeltaLookup(context, resultVar, lookupVar, result)
      case (MappingAccess(Delta(), lookupVar), MappingAccess(DeltaResult(), resultVar)) => deltaResultEqualsDeltaLookup(context, resultVar, lookupVar, result)

    }
  }

  override def variables: ScalaSet[Id] = mapping1.variables ++ mapping2.variables

  override def isHyperTypeConclusion(): Boolean = mapping1.isHyperTypeConclusion() && mapping2.isHyperTypeConclusion()

}
