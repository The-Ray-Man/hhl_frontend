package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.ast.Id
import viper.silicon.state.terms.BinaryOp
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.UnaryExpr
import viper.silicon.state.terms.UnaryOp
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.typing.dsl.HyperType
import viper.silver.parser.PKw.Spec
import viper.HHLVerifier.typing.dsl.Parser.hyperType
import viper.HHLVerifier.typing.HyperMapping
import viper.HHLVerifier.typing.DeltaMapping
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.typing.dsl.Parser.setEquals
import scala.collection.immutable.{Set => ScalaSet}
import viper.HHLVerifier.ast.Stmt
import scala.collection.View.Empty

object SpecificationUtil {

  def combineSpecifications(specifications: Seq[Specification]): Specification = {

    val specificationCombined = specifications.reduce((acc, spec) => {
      SpecificationUtil.mergeSpecification(acc, spec)
    })
    specificationCombined
  }

  def mergeSpecification(spec1: Specification, spec2: Specification): Specification = {
    val hyperTypeDeclarations = spec1.hypertypeDeclaration ++ spec2.hypertypeDeclaration

    val derivationRules = spec1.derivationRules ++ spec2.derivationRules
    val reducedRules    = combineDerivationRules(derivationRules)

    Specification(
      hyperTypeDeclarations,
      reducedRules
    )
  }

  def combineDerivationRules(rules: Seq[DerivationRule]): Seq[DerivationRule] = {
    var expressionRules = Set[ExpressionDerivationRule]()
    var statementRules  = Set[StatementDerivationRule]()
    for (rule <- rules) {
      if (rule.isInstanceOf[ExpressionDerivationRule]) {
        val dupplicatedRule = expressionRules.find(r => canBeCombined(r, rule.asInstanceOf[ExpressionDerivationRule]).isDefined)
        dupplicatedRule match {
          case None        => expressionRules += rule.asInstanceOf[ExpressionDerivationRule]
          case Some(value) => {
            val combinedRule = combineExpressionDerivationRules(value, rule.asInstanceOf[ExpressionDerivationRule]).getOrElse(throw new Exception("123"))
            expressionRules -= value
            expressionRules += combinedRule
          }
        }
      } else {
        val dupplicatedRule = statementRules.find(r => canBeCombined(r, rule.asInstanceOf[StatementDerivationRule]).isDefined)
        dupplicatedRule match {
          case None        => statementRules += rule.asInstanceOf[StatementDerivationRule]
          case Some(value) => {
            val combinedRule = combineStatementDerivationRules(value, rule.asInstanceOf[StatementDerivationRule]).getOrElse(throw new Exception("123"))
            statementRules -= value
            statementRules += combinedRule
          }
        }
      }
    }
    expressionRules.toSeq ++ statementRules.toSeq
  }

  def combineExpressionDerivationRules(rule1: ExpressionDerivationRule, rule2: ExpressionDerivationRule): Option[ExpressionDerivationRule] = {
    canBeCombined(rule1, rule2) match {
      case None                => None
      case Some(mappingIdtoId) => {
        val adaptedRules = rule2.rules.map(rule => applyIndexed.applyIndexed(mappingIdtoId, rule))
        val allRules     = (rule1.rules ++ adaptedRules).toSet.toSeq
        Some(ExpressionDerivationRule(rule1.expr, allRules))
      }
    }
  }

  def combineStatementDerivationRules(rule1: StatementDerivationRule, rule2: StatementDerivationRule): Option[StatementDerivationRule] = {
    canBeCombined(rule1, rule2) match {
      case None                => None
      case Some(mappingIdtoId) => {
        val adaptedRules = rule2.rules.map(rule => applyIndexed.applyIndexed(mappingIdtoId, rule))
        val allRules     = (rule1.rules ++ adaptedRules).toSet.toSeq
        Some(StatementDerivationRule(rule1.statement, allRules))
      }
    }
  }

  def canBeCombined(rule1: ExpressionDerivationRule, rule2: ExpressionDerivationRule): Option[Map[Id, Id]] = {
    (rule1.expr, rule2.expr) match {
      case (Id(name1), Id(name2)) if name1 == name2                                                                               => Some(Map(Id(name1) -> Id(name2)))
      case (BinaryExpr(idLeft1 @ Id(_), op1, idRight1 @ Id(_)), BinaryExpr(idLeft2 @ Id(_), op2, idRight2 @ Id(_))) if op1 == op2 => Some(Map(idLeft1 -> idLeft2, idRight1 -> idRight2))
      case (UnaryExpr(op1, idInner1 @ Id(_)), UnaryExpr(op2, idInner2 @ Id(_))) if op1 == op2                                     => Some(Map(idInner1 -> idInner2))
      case (ImpliesExpr(idLeft1 @ Id(_), idRight1 @ Id(_)), ImpliesExpr(idLeft2 @ Id(_), idRight2 @ Id(_)))                       => Some(Map(idLeft1 -> idLeft2, idRight1 -> idRight2))
      case (LengthExpr(id1 @ Id(_)), LengthExpr(id2 @ Id(_))) if id1 == id2                                                       => Some(Map(id1 -> id2))
      case (LookupExpr(dataStructure1 @ Id(_), index1 @ Id(_)), LookupExpr(dataStructure2 @ Id(_), index2 @ Id(_)))               => Some(Map(dataStructure1 -> dataStructure2, index1 -> index2))
      case _                                                                                                                      => None
    }
  }

  def canBeCombined(rule1: StatementDerivationRule, rule2: StatementDerivationRule): Option[Map[Id, Id]] = {
    (rule1.statement, rule2.statement) match {
      case (AssignStmt(var1, value1), AssignStmt(var2, value2))                               => Some(Map(var1 -> var2, value1 -> value2))
      case (IfStmt(cond1, thenBranch1, elseBranch1), IfStmt(cond2, thenBranch2, elseBranch2)) =>
        Some(Map(cond1 -> cond2, thenBranch1 -> thenBranch2, elseBranch1 -> elseBranch2))
      case (CompStmt(firstStmt1, secondStmt1), CompStmt(firstStmt2, secondStmt2)) =>
        Some(Map(firstStmt1 -> firstStmt2, secondStmt1 -> secondStmt2))
      case _ => None
    }
  }

}

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
      case Variables(content) => Variables(toIndexedVariable(mapping, content))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], map: Mapping): Mapping = {
    map match {
      case Gamma()                                      => Gamma()
      case Delta()                                      => Delta()
      case DeltaCollectionResult()                      => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)           => DeltaTypeCheck(toIndexedVariable(mapping, expr), gamma, delta)
      case GammaResult()                                => GammaResult()
      case DeltaResult()                                => DeltaResult()
      case DeriveHyperType(expr, gamma, delta, context) =>
        DeriveHyperType(toIndexedVariable(mapping, expr), gamma, delta, context)
      case DeriveDeltaType(expr, gamma, delta, context) =>
        DeriveDeltaType(toIndexedVariable(mapping, expr), gamma, delta, context)
      case _: Mapping => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], rule: Rule): Rule = {
    Rule(
      conditions = rule.conditions.map(cond => toIndexedVariable(mapping, cond)),
      conclusions = rule.conclusions.map(concl => toIndexedVariable(mapping, concl))
    )
  }
}

object applyIndexed {

  def applyIndexed(mapping: Map[Id, Id], wrapper: Rule): Rule = {
    Rule(
      conditions = wrapper.conditions.map(cond => applyIndexed(mapping, cond)),
      conclusions = wrapper.conclusions.map(concl => applyIndexed(mapping, concl))
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
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(applyIndexed(mapping, expr), gamma, delta)
      case MappingAccess(subMapping, id)      => MappingAccess(applyIndexed(mapping, subMapping), applyIndexed(mapping, id))
      case WithoutElement(set, elem)          => WithoutElement(applyIndexed(mapping, set), applyIndexed(mapping, elem))
      case Variables(content)                 => Variables(applyIndexed(mapping, content))
    }
  }

  def applyIndexed(mapping: Map[Id, Id], map: Mapping): Mapping = {
    map match {
      case Gamma()                                      => Gamma()
      case Delta()                                      => Delta()
      case DeltaCollectionResult()                      => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)           => DeltaTypeCheck(applyIndexed(mapping, expr), gamma, delta)
      case GammaResult()                                => GammaResult()
      case DeltaResult()                                => DeltaResult()
      case DeriveHyperType(expr, gamma, delta, context) =>
        DeriveHyperType(applyIndexed(mapping, expr), gamma, delta, context)
      case DeriveDeltaType(expr, gamma, delta, context) =>
        DeriveDeltaType(applyIndexed(mapping, expr), gamma, delta, context)
      case _: Mapping => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
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
      case MappingAccess(DeriveDeltaType(id, gammaArg, deltaArg, contextArg), indexId) => {
        val subStatement   = context.getStmtById(id)
        val (gamma, delta) = getArgs(gammaArg, deltaArg)
        val deltaMapping   = context.typeSystem.deriveStatement(gamma, delta, subStatement, context.pc).deltaMapping
        deltaMapping.collection.getOrElse(indexId.name, throw new Exception(s"Index $indexId not found in delta mapping"))
      }
      case _ => throw new Exception("Unsupported mapping type for DeltaCollection retrieval: " + mapping.getClass.getSimpleName)
    }
  }

  def getHyperMapping(mapping: Mapping): HyperMapping = {

    mapping match {
      case DeriveHyperType(id, gammaArg, deltaArg, contextArg) => {
        val stmt     = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getHyperMapping(gammaArg)
        val newDelta = getDeltaMapping(deltaArg)
        context.cache.get(newGamma, newDelta, stmt) match {
          case Some(result) => result.hyperTypeMapping
          case None         => {
            val res = context.typeSystem.deriveStatement(newGamma, newDelta, stmt, context.pc)
            context.cache.add(newGamma, newDelta, stmt, res)
            res.hyperTypeMapping
          }
        }
      }
      case Gamma() => context.gamma
      case _       => throw new Exception("Unsupported mapping type for Gamma condition" + mapping.getClass.getSimpleName)
    }

  }

  def getDeltaMapping(mapping: Mapping): DeltaMapping = {
    mapping match {
      case DeriveDeltaType(id, gammaArg, deltaArg, contextArg) => {
        val stmt     = context.varStmtMapping.getOrElse(id, throw new Exception(s"Variable $id not found in variable mapping"))
        val newGamma = getHyperMapping(gammaArg)
        val newDelta = getDeltaMapping(deltaArg)
        context.cache.get(newGamma, newDelta, stmt) match {
          case Some(result) => result.deltaMapping
          case None         => {
            val res = context.typeSystem.deriveStatement(newGamma, newDelta, stmt, context.pc)
            context.cache.add(newGamma, newDelta, stmt, res)
            res.deltaMapping
          }
        }
      }
      case Delta() => context.delta
      case _       => throw new Exception("Unsupported mapping type for Delta condition" + mapping.getClass.getSimpleName)
    }
  }
}

class Cache() {
  var expressionCache: Map[(Expr, HyperMapping, DeltaMapping), ExpressionDerivationResult] = Map()
  var statementCache: Map[(Stmt, HyperMapping, DeltaMapping), StatementDerivationResult]   = Map()

  def add(gamma: HyperMapping, delta: DeltaMapping, expr: Expr, result: ExpressionDerivationResult): Unit = {
    expressionCache += ((expr, gamma, delta) -> result)
  }
  def add(gamma: HyperMapping, delta: DeltaMapping, stmt: Stmt, result: StatementDerivationResult): Unit = {
    statementCache += ((stmt, gamma, delta) -> result)
  }

  def get(gamma: HyperMapping, delta: DeltaMapping, expr: Expr): Option[ExpressionDerivationResult] = {
    expressionCache.get((expr, gamma, delta))
    // None
  }

  def get(gamma: HyperMapping, delta: DeltaMapping, stmt: Stmt): Option[StatementDerivationResult] = {
    statementCache.get((stmt, gamma, delta))
    // None
  }
}
