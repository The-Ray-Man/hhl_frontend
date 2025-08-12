package viper.HHLVerifier.typing

import viper.HHLVerifier.typing.rules.TypeSystem
import viper.HHLVerifier.typing.rules.RuleWrapper
import viper.HHLVerifier.typing.rules.HyperTypeConclusion
import viper.HHLVerifier.typing.rules.EmptyWrapper
import viper.HHLVerifier.ast.Method
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.ast.CompositeStmt
import viper.HHLVerifier.typing.rules.ForanyVariableWrapper
import viper.HHLVerifier.typing.rules.HyperTypeCondition
import viper.HHLVerifier.ast.Expr
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.dsl.SimpleHyperType
import viper.HHLVerifier.typing.dsl.HyperTypeWithListArgs
import viper.HHLVerifier.typing.dsl.Element
import viper.HHLVerifier.typing.dsl.HyperTypeWithSetArgs
import viper.HHLVerifier.typing.dsl.HyperTypeDeclaration
import viper.HHLVerifier.ast.Hint
import viper.HHLVerifier.ast.MapTupleExpr
import viper.HHLVerifier.ast.CombExpr
import viper.HHLVerifier.ast.SetAssignExpr
import viper.HHLVerifier.ast.AssertVarDecl
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.StateExistsExpr
import viper.HHLVerifier.ast.MethodCallExpr
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.SpecialId
import viper.HHLVerifier.ast.SeqAssignExpr
import viper.HHLVerifier.ast.ImpliesExpr
import viper.HHLVerifier.ast.LengthExpr
import viper.HHLVerifier.ast.MapAssignExpr
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.Assertion
import viper.HHLVerifier.ast.UpdateMapExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.HintDecl
import viper.HHLVerifier.ast.LoopIndex
import upack.Binary



object SoundnessCheck {


    // def checkTypeSystem(system: TypeSystem) : Unit = {
        
    // }

    // def checkExpressionDerivationRule(system: TypeSystem, derivationRule: ExpressionDerivationRule ) : Unit = {
    //     derivationRule match {
    //         case binaryRule: BinaryExpressionDerivationRule => checkBinaryRule(system, binaryRule)
    //         case _ => throw new Exception(s"Unsupported rule type: ${derivationRule.getClass.getName}")
    //     }
    // }

    // def checkBinaryRule(system: TypeSystem, derivationRule: BinaryExpressionDerivationRule) : Unit = {
    //     checkBinaryRule(system, derivationRule.additionRule, "+")
    // }


    // def checkBinaryRule(system: TypeSystem, derivationRule: ExpressionDerivationRule, op: String) : Unit = {
        
    //     val (lhs_type, rhs_type) = op match {
    //         case "+" | "-" | "*" | "/" | "%" => (IntType(), IntType())

    //     }

    //     var lhs = Id("lhs")
    //     lhs.typ = lhs_type
    //     var rhs = Id("rhs")
    //     rhs.typ = rhs_type


    //     // val rules = derivationRule.combineFunctionHypertype

    //     // val testPrograms = rules.rules.map(combFunction => {
    //     //     combFunction match {
    //     //         case EmptyWrapper(binaryFunctionImplication(lhsHTCondition, lhsDTCondition, rhsHTCondition, rhsDTCondition, sideCondition, conclusion)) => {
    //     //             val lhsExpr = lhsHTCondition.map(cond => transformHyperTypeCondition(system, lhs, cond, Map()))
    //     //             val rhsExpr = rhsHTCondition.map(cond => transformHyperTypeCondition(system, rhs, cond, Map()))
    //     //             if (!sideCondition.isEmpty) throw new Exception("Side condition are not yet checked")
    //     //             // val conclusionExpr = conclusion.map(cond => transformHyperTypeCondition(system, ))
    //     //             // val sideConditionExpr = sideCondition.map(cond => transformSideCondition(system, lhs, cond, Map()))
    //     //             null
    //     //         }
    //     //         case ForanyVariableWrapper(rule) => throw new Exception("Forany variable mapper not yet supported")
    //     //     }
            

    //     // val method1 = Method("test", Seq(lhs, rhs), Seq(), Seq(

    //     // ),
    //     // Seq(

    //     // ),
    //     // CompositeStmt(Seq()))
    //     // })
    // }

    // def transformSideCondition(system: TypeSystem, id: Id, condition: SideCondition, mappingPlaceholderToConcrete: Map[Int, Int]) : Expr = {
    //     condition match {
    //         case _ => throw new Exception(s"Unsupported side condition: ${condition.getClass.getName}")
    //     }
    // }

    // def transformHyperTypeConclusion(system: TypeSystem, id: Id, conclusion: HyperTypeConclusion, mappingPlaceholderToConcrete: Map[Int, Int]) : Expr = {
    //     conclusion match {
    //         case ContainsHyperType(hty) => {
    //             hty match {
    //                 case simpleHyperType@SimpleHyperType(name) => {
    //                     val hyperTypeDeclaration = system.hyperTypeDeclaration.find(decl => decl.hty == simpleHyperType).getOrElse(
    //                         throw new Exception(s"Hyper type $name not found in the system")
    //                     )
    //                     val mapping = Map(hyperTypeDeclaration.variable -> id)
    //                     getExpression(hyperTypeDeclaration.definition, mapping)
    //                 }
    //             }
    //         }
    //     }
    // }



    // def transformHyperTypeCondition(system: TypeSystem, id: Id, condition: HyperTypeCondition, mappingPlaceholderToConcrete: Map[Id, Id]) : Expr = {
    //     condition match {
    //         case ElementOf(hty) => {
    //             hty match {
    //                 case simpHyperType@SimpleHyperType(name) => {
    //                     val hyperTypeDeclaration = system.hyperTypeDeclaration.find(decl => decl.hty == simpHyperType).getOrElse(
    //                         throw new Exception(s"Hyper type $name not found in the system")
    //                     )
    //                     val mapping = Map(hyperTypeDeclaration.variable -> id)
    //                     getExpression(hyperTypeDeclaration.definition, mapping)
    //                 }
    //                 case _ => throw new Exception(s"Unsupported hyper type: ${hty.getClass.getName}")
    //             }

    //         }
    //         case _ => throw new Exception(s"Unsupported hyper type condition: ${condition.getClass.getName}")
    //     }
    // }


    def getExpression(htypeDecl: Expr, mappingPlaceholderToConcrete: Map[Id, Id]): Expr = {
        htypeDecl match {
            case Num(value) => htypeDecl
            case UnaryExpr(op, e) => UnaryExpr(op, getExpression(e, mappingPlaceholderToConcrete))
            case StateExistsExpr(state, err) => StateExistsExpr(state, err)
            case BinaryExpr(e1, op, e2) => BinaryExpr(getExpression(e1, mappingPlaceholderToConcrete), op, getExpression(e2, mappingPlaceholderToConcrete))
            case Id(name) => mappingPlaceholderToConcrete.getOrElse(Id(name), throw new Exception("Unmapped Id" ++ name))
            case SeqAssignExpr(elements) => SeqAssignExpr(elements.map(getExpression(_, mappingPlaceholderToConcrete)))
            case ImpliesExpr(left, right) => ImpliesExpr(getExpression(left, mappingPlaceholderToConcrete), getExpression(right, mappingPlaceholderToConcrete))
            case LengthExpr(id) => LengthExpr(getExpression(id, mappingPlaceholderToConcrete))
            case BoolLit(value) => BoolLit(value)
            case Assertion(quantifier, assertVarDecls, body) => htypeDecl
            case LookupExpr(id, index) => getExpressionLookup(id, mappingPlaceholderToConcrete, index)
            case _ => {
                throw new Exception(s"Unsupported expression type: ${htypeDecl.getClass.getName}")
            }
        }
    }

    def getExpressionLookup(stateId: Expr, mappingPlaceholderToConcrete: Map[Id, Id], expr: Expr): Expr = {
        expr match {
            case lookupId@Id(name) => LookupExpr(stateId, mappingPlaceholderToConcrete.getOrElse(lookupId, throw new Exception("Unmapped Id" ++ name)))
            case BinaryExpr(e1, op, e2) => BinaryExpr(getExpressionLookup(stateId, mappingPlaceholderToConcrete, e1), op, getExpressionLookup(stateId, mappingPlaceholderToConcrete, e2))
            case BoolLit(value) => BoolLit(value)
            case Num(value) => Num(value)
            case _ => throw new Exception(s"Unsupported expression type: ${expr.getClass.getName}")
        }
    }


    def findFreeVarBijection(from: Element, to: Element): Option[Map[Id, Id]] = {
        (from, to) match {
            case (fromId@Id(_), toId@Id(_)) => Some(Map(fromId -> toId))
            case (SimpleHyperType(fromName), SimpleHyperType(toName)) => {
                if (fromName == toName) {
                    Some(Map())
                } else {
                    None
                }
            }
            case (HyperTypeWithListArgs(fromNName, fromArgs), HyperTypeWithListArgs(toName, toArgs)) => {
                if (fromNName == toName) {
                    val maps = fromArgs.zip(toArgs).map {
                        case (fromArg, toArg) => {
                            findFreeVarBijection(fromArg, toArg)
                        }
                    }
                    if (maps.forall(_.isDefined)) {
                        maps.foldLeft(Some(Map[Id, Id]()) : Option[Map[Id, Id]]) { (acc, optMap) =>
                            if (acc.isEmpty) {
                                acc
                            } else {
                                combineFreeVarBijection(acc.get, optMap.get)
                            }
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            case (HyperTypeWithSetArgs(fromName, fromArgs), HyperTypeWithSetArgs(toName, toArgs)) => throw new Exception("Set arguments are not supported in this context")
            case _ => None
        }
    }

    def combineFreeVarBijection(mapping: Map[Id, Id], other: Map[Id, Id]): Option[Map[Id, Id]] = {
        var aggregator = Map[Id, Id]()
        for ((fromId, toId) <- mapping) {
            if (aggregator.contains(fromId) && aggregator.get(fromId) != toId) {
                return None
            }
            aggregator += (fromId -> toId)
        }
        for ((fromId, toId) <- other) {
            if (aggregator.contains(fromId) && aggregator.get(fromId) != toId) {
                return None
            }
            aggregator += (fromId -> toId)
        }
        Some(aggregator)
    }
}