package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.typing.dsl.Context
import viper.HHLVerifier.typing.dsl.HyperMapping
import viper.HHLVerifier.typing.dsl.DeltaMapping
import viper.HHLVerifier.typing.dsl.HyperTypeCollection
import viper.HHLVerifier.typing.dsl.ast._
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.HyperTypeChecker.getAssignedVariables
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.dsl.DeltaCollection

case class EvaluateCollection(var context: Context) {

  def getArgs(gamma: Mapping, delta: Mapping): (HyperMapping, DeltaMapping) = {
    val newGamma = getHyperMapping(gamma)
    val newDelta = getDeltaMapping(delta)
    (newGamma, newDelta)
  }

  def getHyperTypeCollection(collection: Set): HyperTypeCollection = {
    collection match {
      case HyperTypeCheck(expr, gammaArg, deltaArg) => {
        val subExpression  = context.varExprMapping.getOrElse(expr, throw new Exception(s"Variable $expr not found in variable mapping"))
        val (gamma, delta) = getArgs(gammaArg, deltaArg)
        context.cache.get(gamma, delta, subExpression) match {
          case Some(result) => result.hyperTypeCollection
          case None         => {
            val res = context.typeSystem.deriveExpression(gamma, delta, subExpression, Map())
            context.cache.add(gamma, delta, subExpression, res)
            res.hyperTypeCollection
          }
        }
      }
      case MappingAccess(mapping, id) => {
        val idIndexed    = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val hyperMapping = getHyperMapping(mapping)
        hyperMapping.get(idIndexed.name)
      }
      case WithoutElement(set, elem) => {
        val derivedCollection = getHyperTypeCollection(set)
        derivedCollection.without(elem.asInstanceOf[HyperType])
      }
      case _ => throw new Exception("Unsupported set type for HyperCollection retrieval: " + collection.getClass.getSimpleName)
    }
  }

  def getDeltaTypes(set: Set): HyperTypeCollection = {
    set match {
      case MappingAccess(mapping, id) => {
        val idIndexed       = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        val deltaCollection = getDeltaCollection(mapping)
        deltaCollection.mapping.getOrElse(idIndexed.name, throw new Exception(s"Index $idIndexed not found in delta mapping"))
      }
    }
  }

  def getDeltaCollection(mapping: Mapping): DeltaCollection = {
    mapping match {
      case DeltaTypeCheck(id, gammaArg, deltaArg) => {
        val subExpression  = context.varExprMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val (gamma, delta) = getArgs(gammaArg, deltaArg)
        context.cache.get(gamma, delta, subExpression) match {
          case Some(result) => result.deltaCollection
          case None         => {
            val res = context.typeSystem.deriveExpression(gamma, delta, subExpression, Map())
            context.cache.add(gamma, delta, subExpression, res)
            res.deltaCollection

          }
        }
      }
      case MappingAccess(DeriveDeltaType(id, gammaArg, deltaArg), indexId) => {
        val subStatement   = context.getStmtById(id)
        val (gamma, delta) = getArgs(gammaArg, deltaArg)
        val deltaMapping   = context.typeSystem.deriveStatement(gamma, delta, subStatement).deltaMapping
        deltaMapping.collection.getOrElse(indexId.name, throw new Exception(s"Index $indexId not found in delta mapping"))
      }
      case MappingAccess(Delta(), id) => {
        val indexedId = context.varExprMapping.getOrElse(id, id).asInstanceOf[Id]
        context.delta.collection.getOrElse(indexedId.name, throw new Exception(s"Index $indexedId not found in delta mapping"))
      }

      case _ => throw new Exception("Unsupported mapping type for DeltaCollection retrieval: " + mapping.getClass.getSimpleName)
    }
  }

  def getHyperMapping(mapping: Mapping): HyperMapping = {

    mapping match {
      case DeriveHyperType(id, gammaArg, deltaArg) => {
        val stmt     = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getHyperMapping(gammaArg)
        val newDelta = getDeltaMapping(deltaArg)
        context.cache.get(newGamma, newDelta, stmt) match {
          case Some(result) => result.hyperTypeMapping
          case None         => {
            val res = context.typeSystem.deriveStatement(newGamma, newDelta, stmt)
            context.cache.add(newGamma, newDelta, stmt, res)
            res.hyperTypeMapping
          }
        }
      }
      case Gamma()                  => context.gamma
      case InitializeGammaMapping() => {
        context.typeSystem.init(context.gamma, context.delta).getStatementResult.hyperTypeMapping
      }
      case _ => throw new Exception("Unsupported mapping type for Gamma condition" + mapping.getClass.getSimpleName)
    }

  }

  def getDeltaMapping(mapping: Mapping): DeltaMapping = {
    mapping match {
      case DeriveDeltaType(id, gammaArg, deltaArg) => {
        val stmt     = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getHyperMapping(gammaArg)
        val newDelta = getDeltaMapping(deltaArg)
        context.cache.get(newGamma, newDelta, stmt) match {
          case Some(result) => result.deltaMapping
          case None         => {
            val res = context.typeSystem.deriveStatement(newGamma, newDelta, stmt)
            context.cache.add(newGamma, newDelta, stmt, res)
            res.deltaMapping
          }
        }
      }
      case Delta()                  => context.delta
      case InitializeDeltaMapping() => {
        context.typeSystem.init(context.gamma, context.delta).getStatementResult.deltaMapping

      }
      case _ => throw new Exception("Unsupported mapping type for Delta condition" + mapping.getClass.getSimpleName)
    }
  }

  def getVariableSet(varSet: Set): ScalaSet[Id] = {
    varSet match {
      case Variables(content) => {
        val _ = context.varExprMapping.get(content) match {
          case Some(expr) => return getVariables(expr)
          case None       => {}
        }
        val _ = context.varStmtMapping.get(content) match {
          case Some(stmt) => return getVariables(stmt)
          case None       => {}
        }
        throw new Exception(s"Variable $content not found in variable mapping")
      }
      case AssignedVariables(stmt) => {
        val statements = context.getStmtById(stmt)
        getAssignedVariables(statements)
      }
      case WithoutElement(set, elem) => {
        val variables = getVariableSet(set)
        variables.excl(elem.asInstanceOf[Id])
      }
      case AllVariables() => {
        context.typeSystem.allVariables
      }
      case AllParameters() => context.typeSystem.allParams
      case _               => throw new Exception("Unsupported variable set type: " + varSet.getClass.getSimpleName)
    }
  }
}
