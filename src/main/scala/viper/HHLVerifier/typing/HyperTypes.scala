package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter
import viper.HHLVerifier.ast.Assertion
import viper.HHLVerifier.generation.State
import viper.silver.{ast => vpr}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.generation.SetState
import viper.HHLVerifier.ast.AssertVarDecl
import viper.HHLVerifier.ast.AssertVar
import viper.HHLVerifier.ast.BinaryExpr
import viper.HHLVerifier.ast.LookupExpr
import viper.HHLVerifier.ast.StateExistsExpr
import viper.HHLVerifier.ast.SpecialId
import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.ast.UnaryExpr
import viper.HHLVerifier.ast.BoolLit
import viper.HHLVerifier.ast.ImpliesExpr
import viper.silicon.state.terms.Greater
import viper.HHLVerifier.typing.rules.RuleCheckContext
import viper.HHLVerifier.typing.dsl.HyperType

// sealed trait HyperType {
//   override def toString: String = {
//     PrettyPrinter.formatHyperType(this)
//   }
//   def deriveTransformId(ruleContext: RuleCheckContext): HyperType
// }

// abstract class MonoHyperType(ids: Set[Id]) extends dsl.HyperType {
//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = {
//     MonoUp(ids.map(i => {
//       if (i.name.toIntOption.isDefined) {
//         val valIndex = i.name.toInt
//         if (ruleContext.variables.length <= valIndex) {
//           throw new Exception(s"Variable with index $valIndex does not exist in the rule Context.")
//         } else {
//           Id(ruleContext.variables(valIndex).name)
//         }
//       } else {
//         throw new Exception("Expected Id with integer name, got: " + i)
//       }
//     }))
//   }
// }
// sealed trait AbsValueHyperType   extends HyperType {}
// sealed trait ValueHyperType      extends HyperType {}
// sealed trait InformationFlowType extends HyperType {}

// case class Low() extends InformationFlowType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s2_assert = AssertVar("_s2")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val s2        = AssertVarDecl(s2_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1, s2),
//       ImpliesExpr(
//         BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)),
//         BinaryExpr(LookupExpr(s1_assert, id), "==", LookupExpr(s2_assert, id))
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, s1VarName: String, STmp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val s1   = State.localVarDecl(s1VarName)
//     val stmt = vpr.Forall(
//       Seq(s0, s1),
//       Seq.empty,
//       vpr.Implies(
//         vpr.And(SetState.getInSetApp(Seq(s0.localVar, STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp)))(),
//         vpr.EqCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class Pos() extends ValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false), BinaryExpr(LookupExpr(s1_assert, id), ">", Num(0))))
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(0)())())())()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class Neg() extends ValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false), BinaryExpr(LookupExpr(s1_assert, id), "<", Num(0))))
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(0)())())())()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class Zero() extends ValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false), BinaryExpr(LookupExpr(s1_assert, id), "==", Num(0))))
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(0)())())())()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class True() extends ValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false), LookupExpr(s1_assert, id)))
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), State.get(s0.localVar, id))())()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class False() extends ValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false), UnaryExpr("!", LookupExpr(s1_assert, id))))
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), vpr.Not(State.get(s0.localVar, id))())())()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class MonoUp(val ids: Set[Id]) extends MonoHyperType(ids) {

//   def semantic(valId: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s2_assert = AssertVar("_s2")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val s2        = AssertVarDecl(s2_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1, s2),
//       ImpliesExpr(
//         BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)),
//         ImpliesExpr(
//           ids.map(id => BinaryExpr(LookupExpr(s1_assert, id), "<=", LookupExpr(s2_assert, id))).reduce((a, b) => BinaryExpr(a, "&&", b)),
//           BinaryExpr(LookupExpr(s1_assert, valId), "<=", LookupExpr(s2_assert, valId))
//         )
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(valId: Id, s0VarName: String, s1VarName: String, STmp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val s1   = State.localVarDecl(s1VarName)
//     val stmt = vpr.Forall(
//       Seq(s0, s1),
//       Seq.empty,
//       vpr.Implies(
//         vpr.And(SetState.getInSetApp(Seq(s0.localVar, STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp)))(),
//         vpr.Implies(
//           ids.map(id => vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()).reduce((a: vpr.Exp, b: vpr.Exp) => vpr.And(a, b)()),
//           // vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))(),
//           vpr.LeCmp(State.get(s0.localVar, valId), State.get(s1.localVar, valId))()
//         )()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }
// case class MonoDown(val ids: Set[Id]) extends MonoHyperType(ids) {
//   def semantic(valId: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s2_assert = AssertVar("_s2")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val s2        = AssertVarDecl(s2_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1, s2),
//       ImpliesExpr(
//         BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)),
//         ImpliesExpr(
//           ids.map(id => BinaryExpr(LookupExpr(s1_assert, id), "<=", LookupExpr(s2_assert, id))).reduce((a, b) => BinaryExpr(a, "&&", b)),
//           BinaryExpr(LookupExpr(s1_assert, valId), ">=", LookupExpr(s2_assert, valId))
//         )
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(valId: Id, s0VarName: String, s1VarName: String, STmp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val s1   = State.localVarDecl(s1VarName)
//     val stmt = vpr.Forall(
//       Seq(s0, s1),
//       Seq.empty,
//       vpr.Implies(
//         vpr.And(SetState.getInSetApp(Seq(s0.localVar, STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp)))(),
//         vpr.Implies(
//           ids.map(id => vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()).reduce((a: vpr.Exp, b: vpr.Exp) => vpr.And(a, b)()),
//           vpr.GeCmp(State.get(s0.localVar, valId), State.get(s1.localVar, valId))()
//         )()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }

// case class GreaterOne() extends AbsValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1),
//       ImpliesExpr(
//         StateExistsExpr(s1_assert, false),
//         BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id), "<", UnaryExpr("-", Num(1))), "||", BinaryExpr(LookupExpr(s1_assert, id), ">", Num(1)))
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(
//       Seq(s0),
//       Seq.empty,
//       vpr.Implies(
//         SetState.getInSetApp(Seq(s0.localVar, STemp)),
//         vpr.Or(vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())(), vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(1)())())()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }
// case class LessOne() extends AbsValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1),
//       ImpliesExpr(
//         StateExistsExpr(s1_assert, false),
//         BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id), ">", UnaryExpr("-", Num(1))), "&&", BinaryExpr(LookupExpr(s1_assert, id), "<", Num(1)))
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(
//       Seq(s0),
//       Seq.empty,
//       vpr.Implies(
//         SetState.getInSetApp(Seq(s0.localVar, STemp)),
//         vpr.And(vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())(), vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(1)())())()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }
// case class One() extends AbsValueHyperType {

//   override def deriveTransformId(ruleContext: RuleCheckContext): HyperType = this

//   def semantic(id: Id): Assertion = {
//     val s1_assert = AssertVar("_s1")
//     val s1        = AssertVarDecl(s1_assert, StateType())
//     val stmt      = Assertion(
//       "forall",
//       Seq(s1),
//       ImpliesExpr(
//         StateExistsExpr(s1_assert, false),
//         BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id), "==", Num(1)), "||", BinaryExpr(LookupExpr(s1_assert, id), "==", UnaryExpr("-", Num(1))))
//       )
//     )
//     return stmt
//   }

//   def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     val s0   = State.localVarDecl(s0VarName)
//     val stmt = vpr.Forall(
//       Seq(s0),
//       Seq.empty,
//       vpr.Implies(
//         SetState.getInSetApp(Seq(s0.localVar, STemp)),
//         vpr.Or(vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(1)())(), vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())())()
//       )()
//     )()
//     (Some(stmt), Seq.empty)
//   }
// }

// object HyperTypes {

//   def semantic_vpr(ty: HyperType, id: Id, s0VarName: String, s1VarName: String, STmp: vpr.LocalVar): (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
//     ty match {
//       case Low()            => Low().semantic_vpr(id, s0VarName, s1VarName, STmp)
//       case Pos()            => Pos().semantic_vpr(id, s0VarName, STmp)
//       case Neg()            => Neg().semantic_vpr(id, s0VarName, STmp)
//       case Zero()           => Zero().semantic_vpr(id, s0VarName, STmp)
//       case False()          => False().semantic_vpr(id, s0VarName, STmp)
//       case True()           => True().semantic_vpr(id, s0VarName, STmp)
//       case MonoDown(baseId) => MonoDown(baseId).semantic_vpr(id, s0VarName, s1VarName, STmp)
//       case MonoUp(baseId)   => MonoUp(baseId).semantic_vpr(id, s0VarName, s1VarName, STmp)
//       case GreaterOne()     => GreaterOne().semantic_vpr(id, s0VarName, STmp)
//       case LessOne()        => LessOne().semantic_vpr(id, s0VarName, STmp)
//       case One()            => One().semantic_vpr(id, s0VarName, STmp)
//     }
//   }
//   def semantic(ty: HyperType, id: Id): Assertion = {
//     ty match {
//       case Low()            => Low().semantic(id)
//       case Pos()            => Pos().semantic(id)
//       case Neg()            => Neg().semantic(id)
//       case Zero()           => Zero().semantic(id)
//       case False()          => False().semantic(id)
//       case True()           => True().semantic(id)
//       case MonoDown(baseId) => MonoDown(baseId).semantic(id)
//       case MonoUp(baseId)   => MonoUp(baseId).semantic(id)
//       case GreaterOne()     => GreaterOne().semantic(id)
//       case LessOne()        => LessOne().semantic(id)
//       case One()            => One().semantic(id)
//     }
//   }
// }

case class HyperTypeCollection(
    val hypertypes: Set[HyperType] = Set.empty[HyperType]
) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.hypertypes == that.hypertypes
      }
      case _ => false
    }
  }

  def isSubTypeOf(other: HyperTypeCollection): Boolean = {
    this.hypertypes.forall(other.hypertypes.contains)
  }

  def add(ty: HyperType): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes + ty)
  }

  def extend(other: HyperTypeCollection): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes ++ other.hypertypes)
  }

  override def toString: String = {
    PrettyPrinter.formatHyperTypeCollection(this)
  }
}

object HyperTypeCollection {

  def fromSeq(seq: Seq[HyperType]): HyperTypeCollection = {

    new HyperTypeCollection(seq.toSet)
  }
}
