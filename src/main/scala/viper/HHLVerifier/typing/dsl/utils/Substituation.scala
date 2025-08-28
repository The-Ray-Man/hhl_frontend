package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.ast.{Id, BinaryExpr, UnaryExpr, ImpliesExpr, LengthExpr, LookupExpr, Expr, Num, BoolLit, MethodCallExpr, CombExpr}
import viper.HHLVerifier.typing.dsl.{HyperType, Element, SimpleHyperType, HyperTypeWithListArgs, HyperTypeWithSetArgs}
import viper.HHLVerifier.typing.dsl.ast._
import viper.HHLVerifier.typing.dsl.HyperTypeCollection

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
