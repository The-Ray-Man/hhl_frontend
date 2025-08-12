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


object SpecificationUtil {

  def combineSpecifications(specifications: Seq[Specification]): Specification = {
    
    val specificationCombined = specifications.reduce((acc, spec) => {
      SpecificationUtil.mergeSpecification(acc, spec)
    })
    specificationCombined
  }

  def mergeSpecification(spec1: Specification, spec2: Specification): Specification = {
    throw new Exception("Not yet implemented: 1. Match the expression, 2. replace the expression in all the rules. 3. Combine rules")
    // val expressionDerivationByOp = (spec1.derivationRules ++ spec2.derivationRules).filter(_.isInstanceOf[ExpressionDerivationRule]).map(_.asInstanceOf[ExpressionDerivationRule]).groupBy(_.op).map(_._2).toSeq
    // val combinedRules = expressionDerivationByOp.map(rules => rules.reduce((rule1, rule2) => combineExpressionDerivationRules(rule1, rule2)))

    // val combinedHypertypeDeclarations = spec1.hypertypeDeclaration ++ spec2.hypertypeDeclaration
    // Specification(combinedHypertypeDeclarations, combinedRules)
  }

  def combineExpressionDerivationRules(rule1 : ExpressionDerivationRule, rule2: ExpressionDerivationRule): ExpressionDerivationRule = {
    throw new Exception("Not yet implemented: 1. Match the expression, 2. replace the expression in all the rules. 3. Combine rules")
    // assert(samePlaceholderExpression(rule1.expr, rule2.expr), "Cannot combine rules with different expressions")
    // val rules = rule1.rules ++ rule2.rules
    // ExpressionDerivationRule(rule1.expr, rules)
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
      case id: Id => toIndexedVariable(mapping, id)
      case BinaryExpr(left, op, right) =>
        BinaryExpr(toIndexedVariable(mapping, left), op, toIndexedVariable(mapping, right))
      case UnaryExpr(op, expr) =>
        UnaryExpr(op, toIndexedVariable(mapping, expr))
      case ImpliesExpr(left, right) =>
        ImpliesExpr(toIndexedVariable(mapping, left), toIndexedVariable(mapping, right))
      case LengthExpr(id) => LengthExpr(toIndexedVariable(mapping, id))
      case LookupExpr(id, index) => LookupExpr(toIndexedVariable(mapping, id), toIndexedVariable(mapping,index))
      case _ => throw new Exception(s"Unsupported expression type for indexing: $expr")
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], elem: Element) : Element = {
    elem match {
      case hty : HyperType => toIndexedVariable(mapping,hty)
      case ident@Id(name) => toIndexedVariable(mapping, ident)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperType) : HyperType = {
    hty match {
      case SimpleHyperType(name) => hty
      case htList@HyperTypeWithListArgs(name, args) => toIndexedVariable(mapping, htList)
      case htSet@HyperTypeWithSetArgs(name, args) => toIndexedVariable(mapping, htSet)
    }
  }
  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperTypeWithListArgs): HyperTypeWithListArgs = {
    HyperTypeWithListArgs(hty.name, hty.args.map(arg => toIndexedVariable(mapping, arg)))
  }
  def toIndexedVariable(mapping: Map[Id, Int], hty: HyperTypeWithSetArgs): HyperTypeWithSetArgs = {
    HyperTypeWithSetArgs(hty.name, hty.args.map(arg => toIndexedVariable(mapping, arg)))
  }

  def toIndexedVariable(mapping: Map[Id, Int], conclusion: Conclusion) : Conclusion = {
    conclusion match {
      case AddToSet(elem, set) => AddToSet(toIndexedVariable(mapping, elem), toIndexedVariable(mapping, set))
      case SetEquals(set1, set2) => SetEquals(toIndexedVariable(mapping, set1), toIndexedVariable(mapping, set2))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], condition: Condition) : Condition = {
    condition match {
      case arithCond : ArithCondition => toIndexedVariable(mapping, arithCond)
      case boolCond : BoolCondition => toIndexedVariable(mapping, boolCond)
      case inSetCond : InSet => toIndexedVariable(mapping, inSetCond)
      case notOperator: NotOperator => NotOperator(toIndexedVariable(mapping, notOperator.condition))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], arithCond: ArithCondition): ArithCondition = {
    ArithCondition(toIndexedVariable(mapping, arithCond.variable), arithCond.op, arithCond.right)
  }

  def toIndexedVariable(mapping: Map[Id, Int], boolCond: BoolCondition): BoolCondition = {
    BoolCondition(toIndexedVariable(mapping, boolCond.variable))
  }

  def toIndexedVariable(mapping: Map[Id, Int], inSet: InSet): InSet = {
    InSet(toIndexedVariable(mapping, inSet.elem), toIndexedVariable(mapping, inSet.set))
  }

  def toIndexedVariable(mapping: Map[Id, Int], set: Set) : Set = {
    set match {
      case HyperCollectionResult() => HyperCollectionResult()
      case HyperTypeCheck(expr, gamma, delta) => HyperTypeCheck(toIndexedVariable(mapping, expr), gamma, delta)
      case MappingAccess(subExpr, id) => MappingAccess(toIndexedVariable(mapping, subExpr), toIndexedVariable(mapping, id))
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], map: Mapping) : Mapping = {
    map match {
      case Gamma() => Gamma()
      case Delta() => Delta()
      case DeltaCollectionResult() => DeltaCollectionResult()
      case _: Mapping => throw new Exception("Unsupported mapping type for indexing: " + map.getClass.getSimpleName)
    }
  }

  def toIndexedVariable(mapping: Map[Id, Int], rule: Rule) : Rule = {
    Rule(
      conditions = rule.conditions.map(cond => toIndexedVariable(mapping, cond)),
      conclusions = rule.conclusions.map(concl => toIndexedVariable(mapping, concl))
    )
  }
}


object applyIndexed {
  def applyIndexed(mapping: Map[Id, Id], elem: Element) : Element = {
    elem match {
      case id@Id(name) => mapping.getOrElse(id, throw new Exception(s"Variable $id not found in mapping"))
      case hty: HyperType => applyIndexed(mapping, hty) 
    }
  }
  def applyIndexed(mapping: Map[Id, Id], hty: HyperType) : HyperType = {
    hty match {
      case SimpleHyperType(name) => hty
      case HyperTypeWithListArgs(name, args) => HyperTypeWithListArgs(name, args.map(arg => applyIndexed(mapping, arg)))
      case HyperTypeWithSetArgs(name, args) => HyperTypeWithSetArgs(name, args.map(arg => applyIndexed(mapping, arg)))
    }
  }
}