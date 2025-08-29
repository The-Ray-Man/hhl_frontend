package viper.HHLVerifier.typing.dsl.utils

import viper.HHLVerifier.ast.{Id, BinaryExpr, UnaryExpr, ImpliesExpr, LengthExpr, LookupExpr, Expr, Num, BoolLit, MethodCallExpr, CombExpr}
import viper.HHLVerifier.typing.dsl.{HyperType, Element, SimpleHyperType, HyperTypeWithListArgs, HyperTypeWithSetArgs}
import viper.HHLVerifier.typing.dsl.ast._
import viper.HHLVerifier.typing.dsl.HyperTypeCollection

/** Utility class to substitute some variables with others. Given the mapping, every occurence of the key is replaced with the value.
  */
case class Substitution(mapping: Map[Id, Id]) {

  /** Creates a new object where the substitution is applied. */
  def apply(wrapper: Rule): Rule = {
    Rule(
      conditions = wrapper.conditions.map(cond => apply(cond)),
      conclusions = wrapper.conclusions.map(concl => apply(concl)),
      name = wrapper.name
    )
  }

  /** Creates a new object where the substitution is applied. */
  def apply(cond: Condition): Condition = {
    cond match {
      case euqalCond: Equal          => Equal(apply(euqalCond.lhs), apply(euqalCond.rhs))
      case arithCond: ArithCondition => ArithCondition(apply(arithCond.variable), arithCond.op, arithCond.right)
      case boolCond: BoolCondition   => BoolCondition(apply(boolCond.variable))
      case inSetCond: InSet          => InSet(apply(inSetCond.elem), apply(inSetCond.set))
      case notOperator: NotOperator  => NotOperator(apply(notOperator.condition))
      case setEquals: SetEquals      => SetEquals(apply(setEquals.set1), apply(setEquals.set2))
      case mapEquals: MapEquals      => MapEquals(apply(mapEquals.mapping1), apply(mapEquals.mapping2))
      case inMappingCond: InMapping  => InMapping(apply(inMappingCond.elem), apply(inMappingCond.mapping))
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(conclusion: Conclusion): Conclusion = {
    conclusion match {
      case AddToSet(elem, set)           => AddToSet(apply(elem), apply(set))
      case SetEquals(set1, set2)         => SetEquals(apply(set1), apply(set2))
      case MapEquals(mapping1, mapping2) => MapEquals(apply(mapping1), apply(mapping2))
      case ExtendSet(toAdd, toExtend)    => ExtendSet(apply(toAdd), apply(toExtend))
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(variable: Id): Id = {
    mapping.getOrElse(variable, variable)
  }

  /** Creates a new object where the substitution is applied. */
  def apply(set: Set): Set = {
    set match {
      case HyperCollectionResult()            => HyperCollectionResult()
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(apply(expr), apply(gamma), apply(delta))
      case MappingAccess(subMapping, id)      => MappingAccess(apply(subMapping), apply(id))
      case WithoutElement(set, elem)          => WithoutElement(apply(set), apply(elem))
      case Variables(content)                 => Variables(apply(content))
      case AssignedVariables(stmt)            => AssignedVariables(apply(stmt))
      case AllParameters()                    => AllParameters()
      case AllVariables()                     => AllVariables()
      case _                                  => throw new Exception("Unsupported set type for indexing: " + set.getClass.getSimpleName)
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(map: Mapping): Mapping = {
    map match {
      case Gamma()                             => Gamma()
      case Delta()                             => Delta()
      case DeltaCollectionResult()             => DeltaCollectionResult()
      case DeltaTypeCheck(expr, gamma, delta)  => DeltaTypeCheck(apply(expr), gamma, delta)
      case GammaResult()                       => GammaResult()
      case DeltaResult()                       => DeltaResult()
      case DeriveHyperType(expr, gamma, delta) =>
        DeriveHyperType(apply(expr), apply(gamma), apply(delta))
      case DeriveDeltaType(expr, gamma, delta) =>
        DeriveDeltaType(apply(expr), apply(gamma), apply(delta))
      case MappingAccess(subMapping, id) => MappingAccess(apply(subMapping), apply(id))
      case InitializeDeltaMapping()      => InitializeDeltaMapping()
      case _: Mapping                    => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(elem: Element): Element = {
    elem match {
      case id @ Id(_)     => apply(id)
      case hty: HyperType => apply(hty)
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(hty: HyperType): HyperType = {
    hty match {
      case SimpleHyperType(name)             => SimpleHyperType(name)
      case HyperTypeWithListArgs(name, args) => HyperTypeWithListArgs(name, args.map(arg => apply(arg)))
      case HyperTypeWithSetArgs(name, args)  => HyperTypeWithSetArgs(name, args.map(arg => apply(arg)))
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(expr: Expr): Expr = {
    expr match {
      case BinaryExpr(left, op, right) =>
        BinaryExpr(apply(left), op, apply(right))
      case UnaryExpr(op, inner) =>
        UnaryExpr(op, apply(inner))
      case Id(name) =>
        mapping.getOrElse(Id(name), Id(name))
      case Num(value) =>
        Num(value)
      case BoolLit(value) =>
        BoolLit(value)
      case ImpliesExpr(left, right) =>
        ImpliesExpr(apply(left), apply(right))
      case MethodCallExpr(methodName, args) =>
        MethodCallExpr(methodName, args.map(arg => apply(arg).asInstanceOf[Id]))
      case LookupExpr(dataStructure, index) =>
        LookupExpr(apply(dataStructure), apply(index))
      case LengthExpr(dataStructure) =>
        LengthExpr(apply(dataStructure))
      case CombExpr(lhs, rhs, op) =>
        CombExpr(apply(lhs), apply(rhs), op)
      case _ => throw new Exception(s"Unsupported expression type for indexing: $expr")
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(stmt: StmtPattern): StmtPattern = {
    stmt match {
      case AssignStmt(variable, value)               => AssignStmt(apply(variable), apply(value))
      case CompStmt(first, second)                   => CompStmt(apply(first), apply(second))
      case IfStmt(condition, thenBranch, elseBranch) => IfStmt(apply(condition), apply(thenBranch), apply(elseBranch))
      case InitStmt()                                => InitStmt()
      case HavocStmt(variable)                       => HavocStmt(apply(variable))
      case MethodInitStmt(variable)                  => MethodInitStmt(apply(variable))
    }
  }

  /** Creates a new object where the substitution is applied. */
  def apply(collection: HyperTypeCollection): HyperTypeCollection = {
    HyperTypeCollection(collection.hypertypes.map(ht => apply(ht)))
  }
}
