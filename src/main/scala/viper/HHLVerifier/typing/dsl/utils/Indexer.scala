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


object ToIndexed {

  def toIndexedVariable(mapping: Map[Id, Int], variable: Id): Id = {
    mapping.get(variable) match {
      case Some(index) => Id(s"<$index>")
      case None        => variable
    }
  }
  def toIndexedVariable(mapping: Map[Id, Int], expr: Expr): Expr = {
    expr match {
      case id: Id                      => toIndexedVariable(mapping, id)
      case BinaryExpr(left, op, right) =>
        BinaryExpr(toIndexedVariable(mapping, left), op, toIndexedVariable(mapping, right))
      case UnaryExpr(op, expr) =>
        UnaryExpr(op, toIndexedVariable(mapping, expr))
      case ImpliesExpr(left, right) =>
        ImpliesExpr(toIndexedVariable(mapping, left), toIndexedVariable(mapping, right))
      case LengthExpr(id)        => LengthExpr(toIndexedVariable(mapping, id))
      case LookupExpr(id, index) => LookupExpr(toIndexedVariable(mapping, id), toIndexedVariable(mapping, index))
      case _                     => throw new Exception(s"Unsupported expression type for indexing: $expr")
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], elem: Element): Element = {
    elem match {
      case hty: HyperType   => toIndexedVariable(mapping, hty)
      case ident @ Id(name) => toIndexedVariable(mapping, ident)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperType): HyperType = {
    hty match {
      case SimpleHyperType(name)                      => hty
      case htList @ HyperTypeWithListArgs(name, args) => toIndexedVariable(mapping, htList)
      case htSet @ HyperTypeWithSetArgs(name, args)   => toIndexedVariable(mapping, htSet)
    }
  }
  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperTypeWithListArgs): HyperTypeWithListArgs = {
    HyperTypeWithListArgs(hty.name, hty.args.map(arg => toIndexedVariable(mapping, arg)))
  }
  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperTypeWithSetArgs): HyperTypeWithSetArgs = {
    HyperTypeWithSetArgs(hty.name, hty.args.map(arg => toIndexedVariable(mapping, arg)))
  }

  def toIndexedVariable(mapping: Map[Id, Int], conclusion: Conclusion): Conclusion = {
    conclusion match {
      case AddToSet(elem, set)           => AddToSet(toIndexedVariable(mapping, elem), toIndexedVariable(mapping, set))
      case SetEquals(set1, set2)         => SetEquals(toIndexedVariable(mapping, set1), toIndexedVariable(mapping, set2))
      case MapEquals(mapping1, mapping2) => MapEquals(toIndexedVariable(mapping, mapping1), toIndexedVariable(mapping, mapping2))
      case ExtendSet(toAdd, toExtend)    => ExtendSet(toIndexedVariable(mapping, toAdd), toIndexedVariable(mapping, toExtend))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], condition: Condition): Condition = {
    condition match {
      case equalCond: Equal          => toIndexedVariable(mapping, equalCond)
      case arithCond: ArithCondition => toIndexedVariable(mapping, arithCond)
      case boolCond: BoolCondition   => toIndexedVariable(mapping, boolCond)
      case inSetCond: InSet          => toIndexedVariable(mapping, inSetCond)
      case inMapping: InMapping      => toIndexedVariable(mapping, inMapping)
      case mapEquals: MapEquals      => toIndexedVariable(mapping, mapEquals)
      case setEquals: SetEquals      => toIndexedVariable(mapping, setEquals)
      case notOperator: NotOperator  => NotOperator(toIndexedVariable(mapping, notOperator.condition))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], equalCond: Equal): Equal = {
    Equal(toIndexedVariable(mapping, equalCond.lhs), toIndexedVariable(mapping, equalCond.rhs))
  }

  def toIndexedVariable(mapping: Map[Id, Int], setEquals: SetEquals): SetEquals = {
    SetEquals(toIndexedVariable(mapping, setEquals.set1), toIndexedVariable(mapping, setEquals.set2))
  }

  def toIndexedVariable(mapping: Map[Id, Int], mapEquals: MapEquals): MapEquals = {
    MapEquals(toIndexedVariable(mapping, mapEquals.mapping1), toIndexedVariable(mapping, mapEquals.mapping2))
  }

  def toIndexedVariable(mapping: Map[Id, Int], arithCond: ArithCondition): ArithCondition = {
    ArithCondition(toIndexedVariable(mapping, arithCond.variable), arithCond.op, arithCond.right)
  }

  def toIndexedVariable(mapping: Map[Id, Int], boolCond: BoolCondition): BoolCondition = {
    BoolCondition(toIndexedVariable(mapping, boolCond.variable))
  }

  def toIndexedVariable(mapping: Map[Id, Int], inMapping: InMapping): InMapping = {
    InMapping(toIndexedVariable(mapping, inMapping.elem), toIndexedVariable(mapping, inMapping.mapping))
  }

  def toIndexedVariable(mapping: Map[Id, Int], inSet: InSet): InSet = {
    InSet(toIndexedVariable(mapping, inSet.elem), toIndexedVariable(mapping, inSet.set))
  }

  def toIndexedVariable(mapping: Map[Id, Int], set: Set): Set = {
    set match {
      case HyperCollectionResult()            => HyperCollectionResult()
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(toIndexedVariable(mapping, expr), gamma, delta)
      case MappingAccess(subExpr, id)         => MappingAccess(toIndexedVariable(mapping, subExpr), toIndexedVariable(mapping, id))
      case WithoutElement(set, elem)          => WithoutElement(toIndexedVariable(mapping, set), toIndexedVariable(mapping, elem))
      case Variables(content)                 => Variables(toIndexedVariable(mapping, content))
      case AssignedVariables(stmt)            => AssignedVariables(toIndexedVariable(mapping, stmt))
      case AllParameters()                    => AllParameters()
      case AllVariables()                     => AllVariables()
      case _                                  => throw new Exception("Unsupported set type for indexing: " + set.getClass.getSimpleName)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], map: Mapping): Mapping = {
    map match {
      case Gamma()                             => Gamma()
      case Delta()                             => Delta()
      case DeltaCollectionResult()             => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)  => DeltaTypeCheck(toIndexedVariable(mapping, expr), gamma, delta)
      case GammaResult()                       => GammaResult()
      case DeltaResult()                       => DeltaResult()
      case DeriveHyperType(expr, gamma, delta) =>
        DeriveHyperType(toIndexedVariable(mapping, expr), gamma, delta)
      case DeriveDeltaType(expr, gamma, delta) =>
        DeriveDeltaType(toIndexedVariable(mapping, expr), gamma, delta)
      case MappingAccess(subMapping, id) => MappingAccess(toIndexedVariable(mapping, subMapping), toIndexedVariable(mapping, id))
      case InitializeGammaMapping()      => InitializeGammaMapping()
      case InitializeDeltaMapping()      => InitializeDeltaMapping()
      case _: Mapping                    => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], rule: Rule): Rule = {
    Rule(
      conditions = rule.conditions.map(cond => toIndexedVariable(mapping, cond)),
      conclusions = rule.conclusions.map(concl => toIndexedVariable(mapping, concl)),
      name = rule.name
    )
  }
}

object applyIndexed {

  def applyIndexed(mapping: Map[Id, Id], wrapper: Rule): Rule = {
    Rule(
      conditions = wrapper.conditions.map(cond => applyIndexed(mapping, cond)),
      conclusions = wrapper.conclusions.map(concl => applyIndexed(mapping, concl)),
      name = wrapper.name
    )
  }

  def applyIndexed(mapping: Map[Id, Id], cond: Condition): Condition = {
    cond match {
      case euqalCond: Equal          => Equal(applyIndexed(mapping, euqalCond.lhs), applyIndexed(mapping, euqalCond.rhs))
      case arithCond: ArithCondition => ArithCondition(applyIndexed(mapping, arithCond.variable), arithCond.op, arithCond.right)
      case boolCond: BoolCondition   => BoolCondition(applyIndexed(mapping, boolCond.variable))
      case inSetCond: InSet          => InSet(applyIndexed(mapping, inSetCond.elem), applyIndexed(mapping, inSetCond.set))
      case notOperator: NotOperator  => NotOperator(applyIndexed(mapping, notOperator.condition))
      case setEquals: SetEquals      => SetEquals(applyIndexed(mapping, setEquals.set1), applyIndexed(mapping, setEquals.set2))
      case mapEquals: MapEquals      => MapEquals(applyIndexed(mapping, mapEquals.mapping1), applyIndexed(mapping, mapEquals.mapping2))
      case inMappingCond: InMapping  => InMapping(applyIndexed(mapping, inMappingCond.elem), applyIndexed(mapping, inMappingCond.mapping))
    }
  }

  def applyIndexed(mapping: Map[Id, Id], conclusion: Conclusion): Conclusion = {
    conclusion match {
      case AddToSet(elem, set)           => AddToSet(applyIndexed(mapping, elem), applyIndexed(mapping, set))
      case SetEquals(set1, set2)         => SetEquals(applyIndexed(mapping, set1), applyIndexed(mapping, set2))
      case MapEquals(mapping1, mapping2) => MapEquals(applyIndexed(mapping, mapping1), applyIndexed(mapping, mapping2))
      case ExtendSet(toAdd, toExtend)    => ExtendSet(applyIndexed(mapping, toAdd), applyIndexed(mapping, toExtend))
    }
  }

  def applyIndexed(mapping: Map[Id, Id], variable: Id): Id = {
    mapping.getOrElse(variable, variable)
  }

  def applyIndexed(mapping: Map[Id, Id], set: Set): Set = {
    set match {
      case HyperCollectionResult()            => HyperCollectionResult()
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(applyIndexed(mapping, expr), applyIndexed(mapping, gamma), applyIndexed(mapping, delta))
      case MappingAccess(subMapping, id)      => MappingAccess(applyIndexed(mapping, subMapping), applyIndexed(mapping, id))
      case WithoutElement(set, elem)          => WithoutElement(applyIndexed(mapping, set), applyIndexed(mapping, elem))
      case Variables(content)                 => Variables(applyIndexed(mapping, content))
      case AssignedVariables(stmt)            => AssignedVariables(applyIndexed(mapping, stmt))
      case AllParameters()                    => AllParameters()
      case AllVariables()                     => AllVariables()
      case _                                  => throw new Exception("Unsupported set type for indexing: " + set.getClass.getSimpleName)
    }
  }

  def applyIndexed(mapping: Map[Id, Id], map: Mapping): Mapping = {
    map match {
      case Gamma()                             => Gamma()
      case Delta()                             => Delta()
      case DeltaCollectionResult()             => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)  => DeltaTypeCheck(applyIndexed(mapping, expr), gamma, delta)
      case GammaResult()                       => GammaResult()
      case DeltaResult()                       => DeltaResult()
      case DeriveHyperType(expr, gamma, delta) =>
        DeriveHyperType(applyIndexed(mapping, expr), applyIndexed(mapping, gamma), applyIndexed(mapping, delta))
      case DeriveDeltaType(expr, gamma, delta) =>
        DeriveDeltaType(applyIndexed(mapping, expr), applyIndexed(mapping, gamma), applyIndexed(mapping, delta))
      case MappingAccess(subMapping, id) => MappingAccess(applyIndexed(mapping, subMapping), applyIndexed(mapping, id))
      case InitializeDeltaMapping()      => InitializeDeltaMapping()
      case _: Mapping                    => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  def applyIndexed(mapping: Map[Id, Id], elem: Element): Element = {
    elem match {
      case id @ Id(name)  => mapping.getOrElse(id, id)
      case hty: HyperType => applyIndexed(mapping, hty)
    }
  }
  def applyIndexed(mapping: Map[Id, Id], hty: HyperType): HyperType = {
    hty match {
      case SimpleHyperType(name)             => hty
      case HyperTypeWithListArgs(name, args) => HyperTypeWithListArgs(name, args.map(arg => applyIndexed(mapping, arg)))
      case HyperTypeWithSetArgs(name, args)  => HyperTypeWithSetArgs(name, args.map(arg => applyIndexed(mapping, arg)))
    }
  }
  def applyIndexed(mapping: Map[Id, Id], expr: Expr): Expr = {
    expr match {
      case BinaryExpr(left, op, right) =>
        BinaryExpr(applyIndexed(mapping, left), op, applyIndexed(mapping, right))
      case UnaryExpr(op, inner) =>
        UnaryExpr(op, applyIndexed(mapping, inner))
      case Id(name) =>
        mapping.getOrElse(Id(name), Id(name))
      case Num(value) =>
        Num(value)
      case BoolLit(value) =>
        BoolLit(value)
      case ImpliesExpr(left, right) =>
        ImpliesExpr(applyIndexed(mapping, left), applyIndexed(mapping, right))
      case MethodCallExpr(methodName, args) =>
        MethodCallExpr(methodName, args.map(arg => applyIndexed(mapping, arg).asInstanceOf[Id]))
      case LookupExpr(dataStructure, index) =>
        LookupExpr(applyIndexed(mapping, dataStructure), applyIndexed(mapping, index))
      case LengthExpr(dataStructure) =>
        LengthExpr(applyIndexed(mapping, dataStructure))
      case CombExpr(lhs, rhs, op) =>
        CombExpr(applyIndexed(mapping, lhs), applyIndexed(mapping, rhs), op)
      case _ => throw new Exception(s"Unsupported expression type for indexing: $expr")
    }
  }

  def applyIndexed(mapping: Map[Id, Id], stmt: StmtPattern): StmtPattern = {
    stmt match {
      case AssignStmt(variable, value)               => AssignStmt(applyIndexed(mapping, variable), applyIndexed(mapping, value))
      case CompStmt(first, second)                   => CompStmt(applyIndexed(mapping, first), applyIndexed(mapping, second))
      case IfStmt(condition, thenBranch, elseBranch) => IfStmt(applyIndexed(mapping, condition), applyIndexed(mapping, thenBranch), applyIndexed(mapping, elseBranch))
      case InitStmt()                                => InitStmt()
      case HavocStmt(variable)                       => HavocStmt(applyIndexed(mapping, variable))
      case MethodInitStmt(variable)                  => MethodInitStmt(applyIndexed(mapping, variable))
    }
  }

  def applyIndexed(mapping: Map[Id, Id], collection: HyperTypeCollection): HyperTypeCollection = {
    HyperTypeCollection(collection.hypertypes.map(ht => applyIndexed(mapping, ht)))
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
