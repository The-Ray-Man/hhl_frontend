package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.typing.dsl._
import viper.HHLVerifier.ast.{Id, Num, BoolLit}
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping

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
        val variableLookup = context.varExprMapping.getOrElse(variable, variable).asInstanceOf[Id]
        val hyperTypes = result.getStatementResult.hyperTypeMapping.mapping.getOrElse(variableLookup.name, HyperTypeCollection(Set()))
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

case class SetEquals(set1: Set, set2: Set) extends Conclusion {

  def hyperTypeResultToGammaLookup(context: Context, result: DerivationResult, index: Id): DerivationResult = {
    context.varExprMapping.getOrElse(index, throw new Exception(s"Variable $index not found in variable mapping")) match {
      case Id(name) => {
        val hyperTypes = context.gamma.getUnsafe(name)
        ExpressionDerivationResult(
          hyperTypeCollection = hyperTypes,
          deltaCollection = result.getExpressionResult.deltaCollection
        )
      }
      case _ => throw new Exception("Expected Id for index in MappingAccess")
    }
  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (set1, set2) match {
      case (HyperCollectionResult(), MappingAccess(Gamma(), index)) => hyperTypeResultToGammaLookup(context, result, index)
      case (MappingAccess(Gamma(), index), HyperCollectionResult()) => hyperTypeResultToGammaLookup(context, result, index)
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

case class MapEquals(mapping1: Mapping, mapping2: Mapping) extends Conclusion {


  def gammaResultEqualsDeriveHyperType(context: Context, deriveHyperType: DeriveHyperType, result: DerivationResult): DerivationResult = {
    val subStatement = context.varStmtMapping.getOrElse(deriveHyperType.expr, throw new Exception(s"Variable ${deriveHyperType.expr} not found in variable mapping"))
    
    val gamma = ConclusionUtils.getGamma(context, deriveHyperType.gamma)
    val delta = ConclusionUtils.getDelta(context, deriveHyperType.delta)
    
    val derivedHyperMapping = context.typeSystem.deriveStatement(gamma, delta, subStatement, context.pc).hyperTypeMapping
    StatementDerivationResult(
      hyperTypeMapping = derivedHyperMapping,
      deltaMapping = result.getStatementResult.deltaMapping
    )
  }

  def gammaResultEqualsDeriveDeltaType(context: Context, deriveDeltaType: DeriveDeltaType, result: DerivationResult): DerivationResult = {
    val subStatement = context.varStmtMapping.getOrElse(deriveDeltaType.expr, throw new Exception(s"Variable ${deriveDeltaType.expr} not found in variable mapping"))
    val gamma = ConclusionUtils.getGamma(context, deriveDeltaType.gamma)
    val delta = ConclusionUtils.getDelta(context, deriveDeltaType.delta)
    
    val derivedDeltaMapping = context.typeSystem.deriveStatement(gamma, delta, subStatement, context.pc).deltaMapping
    StatementDerivationResult(
      hyperTypeMapping = result.getStatementResult.hyperTypeMapping,
      deltaMapping = derivedDeltaMapping
    )
  }

  override def apply(context: Context, result: DerivationResult): DerivationResult = {
    (mapping1, mapping2) match {
      case (GammaResult(), d : DeriveHyperType)  => gammaResultEqualsDeriveHyperType(context, d, result)
      case (d : DeriveHyperType, GammaResult())=> gammaResultEqualsDeriveHyperType(context, d, result)
      case (DeltaResult(), d : DeriveDeltaType) => gammaResultEqualsDeriveDeltaType(context, d, result)
      case (d : DeriveDeltaType, DeltaResult()) => gammaResultEqualsDeriveDeltaType(context, d, result)
    }
  }

  override def variables: ScalaSet[Id] = mapping1.variables ++ mapping2.variables

  override def isHyperTypeConclusion(): Boolean = mapping1.isHyperTypeConclusion() && mapping2.isHyperTypeConclusion()

}


object ConclusionUtils {

  def getGamma(context: Context, gamma: Mapping) : HyperMapping = {

    gamma match {
      case DeriveHyperType(id, gammaArg, deltaArg, contextArg) => {
        val stmt = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getGamma(context, gammaArg)
        val newDelta = getDelta(context, deltaArg)
        context.typeSystem.deriveStatement(newGamma, newDelta, stmt, context.pc).hyperTypeMapping
      }
      case Gamma() => context.gamma
      case _ => throw new Exception("Unsupported mapping type for Gamma condition" + gamma.getClass.getSimpleName)
    }

  }


  def getDelta(context: Context, delta: Mapping) : DeltaMapping = {
    delta match {
      case DeriveDeltaType(id, gammaArg, deltaArg, contextArg) => {
        val stmt = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getGamma(context, gammaArg)
        val newDelta = getDelta(context, deltaArg)
        context.typeSystem.deriveStatement(newGamma, newDelta, stmt, context.pc).deltaMapping
      }
      case Delta() => context.delta
      case _ => throw new Exception("Unsupported mapping type for Delta condition" + delta.getClass.getSimpleName)
    }
  }
}