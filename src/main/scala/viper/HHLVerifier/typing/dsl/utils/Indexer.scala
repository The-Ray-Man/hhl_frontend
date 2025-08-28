package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.ast.{Id, BinaryExpr, UnaryExpr, ImpliesExpr, LengthExpr, LookupExpr, Expr, Num, BoolLit, MethodCallExpr, CombExpr}
import viper.HHLVerifier.typing.dsl.{HyperType, Element, SimpleHyperType, HyperTypeWithListArgs, HyperTypeWithSetArgs}
import viper.HHLVerifier.typing.dsl.ast._
import viper.HHLVerifier.typing.dsl.HyperTypeCollection
import viper.HHLVerifier.typing.dsl.Context
import viper.HHLVerifier.typing.dsl.HyperMapping
import viper.HHLVerifier.typing.dsl.DeltaMapping
import viper.HHLVerifier.typing.dsl.DeltaCollection
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.typing.HyperTypeChecker.getVariables
import viper.HHLVerifier.typing.HyperTypeChecker.getAssignedVariables

object Substitution {

  def apply(mapping: Map[Id, Id], wrapper: Rule): Rule = {
    Rule(
      conditions = wrapper.conditions.map(cond => apply(mapping, cond)),
      conclusions = wrapper.conclusions.map(concl => apply(mapping, concl)),
      name = wrapper.name
    )
  }

  def apply(mapping: Map[Id, Id], cond: Condition): Condition = {
    cond match {
      case euqalCond: Equal          => Equal(apply(mapping, euqalCond.lhs), apply(mapping, euqalCond.rhs))
      case arithCond: ArithCondition => ArithCondition(apply(mapping, arithCond.variable), arithCond.op, arithCond.right)
      case boolCond: BoolCondition   => BoolCondition(apply(mapping, boolCond.variable))
      case inSetCond: InSet          => InSet(apply(mapping, inSetCond.elem), apply(mapping, inSetCond.set))
      case notOperator: NotOperator  => NotOperator(apply(mapping, notOperator.condition))
      case setEquals: SetEquals      => SetEquals(apply(mapping, setEquals.set1), apply(mapping, setEquals.set2))
      case mapEquals: MapEquals      => MapEquals(apply(mapping, mapEquals.mapping1), apply(mapping, mapEquals.mapping2))
      case inMappingCond: InMapping  => InMapping(apply(mapping, inMappingCond.elem), apply(mapping, inMappingCond.mapping))
    }
  }

  def apply(mapping: Map[Id, Id], conclusion: Conclusion): Conclusion = {
    conclusion match {
      case AddToSet(elem, set)           => AddToSet(apply(mapping, elem), apply(mapping, set))
      case SetEquals(set1, set2)         => SetEquals(apply(mapping, set1), apply(mapping, set2))
      case MapEquals(mapping1, mapping2) => MapEquals(apply(mapping, mapping1), apply(mapping, mapping2))
      case ExtendSet(toAdd, toExtend)    => ExtendSet(apply(mapping, toAdd), apply(mapping, toExtend))
    }
  }

  def apply(mapping: Map[Id, Id], variable: Id): Id = {
    mapping.getOrElse(variable, variable)
  }

  def apply(mapping: Map[Id, Id], set: Set): Set = {
    set match {
      case HyperCollectionResult()            => HyperCollectionResult()
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(apply(mapping, expr), apply(mapping, gamma), apply(mapping, delta))
      case MappingAccess(subMapping, id)      => MappingAccess(apply(mapping, subMapping), apply(mapping, id))
      case WithoutElement(set, elem)          => WithoutElement(apply(mapping, set), apply(mapping, elem))
      case Variables(content)                 => Variables(apply(mapping, content))
      case AssignedVariables(stmt)            => AssignedVariables(apply(mapping, stmt))
      case AllParameters()                    => AllParameters()
      case AllVariables()                     => AllVariables()
      case _                                  => throw new Exception("Unsupported set type for indexing: " + set.getClass.getSimpleName)
    }
  }

  def apply(mapping: Map[Id, Id], map: Mapping): Mapping = {
    map match {
      case Gamma()                             => Gamma()
      case Delta()                             => Delta()
      case DeltaCollectionResult()             => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)  => DeltaTypeCheck(apply(mapping, expr), gamma, delta)
      case GammaResult()                       => GammaResult()
      case DeltaResult()                       => DeltaResult()
      case DeriveHyperType(expr, gamma, delta) =>
        DeriveHyperType(apply(mapping, expr), apply(mapping, gamma), apply(mapping, delta))
      case DeriveDeltaType(expr, gamma, delta) =>
        DeriveDeltaType(apply(mapping, expr), apply(mapping, gamma), apply(mapping, delta))
      case MappingAccess(subMapping, id) => MappingAccess(apply(mapping, subMapping), apply(mapping, id))
      case InitializeDeltaMapping()      => InitializeDeltaMapping()
      case _: Mapping                    => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  def apply(mapping: Map[Id, Id], elem: Element): Element = {
    elem match {
      case id @ Id(name)  => mapping.getOrElse(id, id)
      case hty: HyperType => apply(mapping, hty)
    }
  }
  def apply(mapping: Map[Id, Id], hty: HyperType): HyperType = {
    hty match {
      case SimpleHyperType(name)             => hty
      case HyperTypeWithListArgs(name, args) => HyperTypeWithListArgs(name, args.map(arg => apply(mapping, arg)))
      case HyperTypeWithSetArgs(name, args)  => HyperTypeWithSetArgs(name, args.map(arg => apply(mapping, arg)))
    }
  }
  def apply(mapping: Map[Id, Id], expr: Expr): Expr = {
    expr match {
      case BinaryExpr(left, op, right) =>
        BinaryExpr(apply(mapping, left), op, apply(mapping, right))
      case UnaryExpr(op, inner) =>
        UnaryExpr(op, apply(mapping, inner))
      case Id(name) =>
        mapping.getOrElse(Id(name), Id(name))
      case Num(value) =>
        Num(value)
      case BoolLit(value) =>
        BoolLit(value)
      case ImpliesExpr(left, right) =>
        ImpliesExpr(apply(mapping, left), apply(mapping, right))
      case MethodCallExpr(methodName, args) =>
        MethodCallExpr(methodName, args.map(arg => apply(mapping, arg).asInstanceOf[Id]))
      case LookupExpr(dataStructure, index) =>
        LookupExpr(apply(mapping, dataStructure), apply(mapping, index))
      case LengthExpr(dataStructure) =>
        LengthExpr(apply(mapping, dataStructure))
      case CombExpr(lhs, rhs, op) =>
        CombExpr(apply(mapping, lhs), apply(mapping, rhs), op)
      case _ => throw new Exception(s"Unsupported expression type for indexing: $expr")
    }
  }

  def apply(mapping: Map[Id, Id], stmt: StmtPattern): StmtPattern = {
    stmt match {
      case AssignStmt(variable, value)               => AssignStmt(apply(mapping, variable), apply(mapping, value))
      case CompStmt(first, second)                   => CompStmt(apply(mapping, first), apply(mapping, second))
      case IfStmt(condition, thenBranch, elseBranch) => IfStmt(apply(mapping, condition), apply(mapping, thenBranch), apply(mapping, elseBranch))
      case InitStmt()                                => InitStmt()
      case HavocStmt(variable)                       => HavocStmt(apply(mapping, variable))
      case MethodInitStmt(variable)                  => MethodInitStmt(apply(mapping, variable))
    }
  }

  def apply(mapping: Map[Id, Id], collection: HyperTypeCollection): HyperTypeCollection = {
    HyperTypeCollection(collection.hypertypes.map(ht => apply(mapping, ht)))
  }
}

case class DeriveArgsUtils(var context: Context) {

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
