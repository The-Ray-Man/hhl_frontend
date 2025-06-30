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

sealed trait HyperType {
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }
}

sealed trait MonoHyperType extends HyperType {}
sealed trait AbsValueHyperType extends HyperType {}

case class Low() extends HyperType {
  def semantic(id: Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s2_assert = AssertVar("_s2")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val s2 = AssertVarDecl(s2_assert, StateType())
    val stmt = Assertion("forall", Seq(s1,s2), ImpliesExpr(BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)),BinaryExpr(LookupExpr(s1_assert, id),"==",LookupExpr(s2_assert, id)) ))
    return stmt
  }

  def semantic_vpr(id : Id, s0VarName: String, s1VarName:String, STmp : vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val s1 = State.localVarDecl(s1VarName)
    val stmt = vpr.Forall(Seq(s0, s1), Seq.empty, 
        vpr.Implies(vpr.And(SetState.getInSetApp(Seq(s0.localVar,STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp))
        )(), vpr.EqCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()
      )()
      )()
      (Some(stmt), Seq.empty)
  }
} 
case class High() extends HyperType {

  def semantic() : Assertion = {
    // High does not have a semantic, it is just a placeholder
    Assertion("forall", Seq.empty, BoolLit(true))
  }

  def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    // High does not have a semantic, it is just a placeholder
    (None, Seq.empty)
  }

}

case class Pos() extends HyperType {

  def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(LookupExpr(s1_assert, id),">",Num(0))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(0)())()
    )())()
    (Some(stmt), Seq.empty)
  }
}

case class Neg() extends HyperType {

  def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(LookupExpr(s1_assert, id),"<",Num(0))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(0)())()
    )())()
    (Some(stmt), Seq.empty)
  }
}

case class Zero() extends HyperType {
  def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(LookupExpr(s1_assert, id),"==",Num(0))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(0)())()
    )())()
    (Some(stmt), Seq.empty)
  }
}

case class True() extends HyperType {
  def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) , LookupExpr(s1_assert, id)))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), State.get(s0.localVar, id))())()
    (Some(stmt), Seq.empty)
  }
}

case class False() extends HyperType {
 def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) , UnaryExpr("!", LookupExpr(s1_assert, id))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), vpr.Not(State.get(s0.localVar, id))())())()
    (Some(stmt), Seq.empty)
  }
}

case class MonoUp(val ids : Set[Id]) extends MonoHyperType {
  def semantic(valId: Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s2_assert = AssertVar("_s2")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val s2 = AssertVarDecl(s2_assert, StateType())
    val stmt = Assertion("forall", Seq(s1,s2), ImpliesExpr(BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)) ,
      ImpliesExpr(
        ids.map(id => BinaryExpr(LookupExpr(s1_assert, id),"<=",LookupExpr(s2_assert, id))).reduce((a,b) => BinaryExpr(a,"&&",b)),
        BinaryExpr(LookupExpr(s1_assert, valId),"<=",LookupExpr(s2_assert, valId))
      )
     ))
    return stmt
  }

  def semantic_vpr(valId : Id, s0VarName: String, s1VarName:String, STmp : vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val s1 = State.localVarDecl(s1VarName)
    val stmt = vpr.Forall(Seq(s0, s1), Seq.empty, 
        vpr.Implies(vpr.And(SetState.getInSetApp(Seq(s0.localVar,STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp))
        )(), vpr.Implies(
          ids.map(id => vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()).reduce((a: vpr.Exp,b : vpr.Exp) => vpr.And(a,b)()),
          // vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))(),
          vpr.LeCmp(State.get(s0.localVar, valId), State.get(s1.localVar, valId))()
        )()
      )()
      )()
      (Some(stmt), Seq.empty)
  }
}
case class MonoDown(val ids : Set[Id]) extends MonoHyperType {
def semantic(valId: Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s2_assert = AssertVar("_s2")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val s2 = AssertVarDecl(s2_assert, StateType())
    val stmt = Assertion("forall", Seq(s1,s2), ImpliesExpr(BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)) ,
      ImpliesExpr(
        ids.map(id => BinaryExpr(LookupExpr(s1_assert, id),"<=",LookupExpr(s2_assert, id))).reduce((a,b) => BinaryExpr(a,"&&",b)),
        BinaryExpr(LookupExpr(s1_assert, valId),">=",LookupExpr(s2_assert, valId))
      )
     ))
    return stmt
  }

  def semantic_vpr(valId : Id, s0VarName: String, s1VarName:String, STmp : vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val s1 = State.localVarDecl(s1VarName)
    val stmt = vpr.Forall(Seq(s0, s1), Seq.empty, 
        vpr.Implies(vpr.And(SetState.getInSetApp(Seq(s0.localVar,STmp)), SetState.getInSetApp(Seq(s1.localVar, STmp))
        )(), vpr.Implies(
          ids.map(id => vpr.LeCmp(State.get(s0.localVar, id), State.get(s1.localVar, id))()).reduce((a: vpr.Exp,b : vpr.Exp) => vpr.And(a,b)()),
          vpr.GeCmp(State.get(s0.localVar, valId), State.get(s1.localVar, valId))()
        )()
      )()
      )()
      (Some(stmt), Seq.empty)
  }
}

case class MonoTypeCollection(val mono: MonoHyperType) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: MonoTypeCollection => this.mono == that.mono
      case _ => false
    }
  }

  def keySet: Set[Id] = {
    // Extract the set of Ids from the MonoHyperType
    this.mono match {
      case MonoUp(ids) => ids
      case MonoDown(ids) => ids
    }
  }


  def subsetOf(other: MonoTypeCollection): Boolean = {
    // Check if this mono type collection is a subset of the other
    this == other
  }

  def flip: MonoTypeCollection = {
    // Flip the mono types, i.e., change MonoUp to MonoDown and vice versa
    this.mono match {
      case MonoUp(ids) => MonoTypeCollection(MonoDown(ids))
      case MonoDown(ids) => MonoTypeCollection(MonoUp(ids))
    }
  }

  def union(other: MonoTypeCollection): Option[MonoTypeCollection] = {
    // Union of two mono type collections
    (this.mono, other.mono) match {
      case (MonoUp(ids1), MonoUp(ids2)) => Some(MonoTypeCollection(MonoUp(ids1 ++ ids2)))
      case (MonoDown(ids1), MonoDown(ids2)) => Some(MonoTypeCollection(MonoDown(ids1 ++ ids2)))
      case _ => None
    }
  }
}


case class GreaterOne() extends AbsValueHyperType {
  def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id),"<",UnaryExpr("-",Num(1))), "||", BinaryExpr(LookupExpr(s1_assert, id),">",Num(1)))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.And(vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())(),vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(1)())())()
    )())()
    (Some(stmt), Seq.empty)
  }
}
case class LessOne() extends AbsValueHyperType {
    def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id),">",UnaryExpr("-",Num(1))), "||", BinaryExpr(LookupExpr(s1_assert, id),"<",Num(1)))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.And(vpr.GtCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())(),vpr.LtCmp(State.get(s0.localVar, id), vpr.IntLit(1)())())()
    )())()
    (Some(stmt), Seq.empty)
  }
}
case class One() extends AbsValueHyperType {
    def semantic(id : Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val stmt = Assertion("forall", Seq(s1), ImpliesExpr(StateExistsExpr(s1_assert, false) ,BinaryExpr(BinaryExpr(LookupExpr(s1_assert, id),"==",Num(1)), "||", BinaryExpr(LookupExpr(s1_assert, id),"==",UnaryExpr("-",Num(1))))))
    return stmt
  }

  def semantic_vpr(id: Id, s0VarName: String, STemp: vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    val s0 = State.localVarDecl(s0VarName)
    val stmt = vpr.Forall(Seq(s0), Seq.empty, vpr.Implies(SetState.getInSetApp(Seq(s0.localVar, STemp)), 
      vpr.Or(vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(1)())(), 
        vpr.EqCmp(State.get(s0.localVar, id), vpr.IntLit(-1)())())()
      )()
    )()
    (Some(stmt), Seq.empty)
  }
}


object HyperTypes {

  def semantic_vpr(ty: HyperType, id: Id, s0VarName: String, s1VarName:String, STmp : vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    ty match {
        case High() =>  High().semantic_vpr()
        case Low() => Low().semantic_vpr(id, s0VarName, s1VarName, STmp)
        case Pos() => Pos().semantic_vpr(id, s0VarName, STmp)
        case Neg() => Neg().semantic_vpr(id, s0VarName, STmp)
        case Zero() => Zero().semantic_vpr(id, s0VarName, STmp)
        case False() => False().semantic_vpr(id, s0VarName, STmp)
        case True() => True().semantic_vpr(id, s0VarName, STmp)
        case MonoDown(baseId) => MonoDown(baseId).semantic_vpr(id, s0VarName, s1VarName, STmp)
        case MonoUp(baseId) => MonoUp(baseId).semantic_vpr(id, s0VarName, s1VarName, STmp) 
        case GreaterOne() => GreaterOne().semantic_vpr(id, s0VarName, STmp)
        case LessOne() => LessOne().semantic_vpr(id, s0VarName, STmp)
        case One() => One().semantic_vpr(id, s0VarName, STmp)
    }
  }
  def semantic(ty: HyperType, id: Id) : Assertion = {
    ty match {
      case High() => High().semantic()
      case Low() => Low().semantic(id)
      case Pos() => Pos().semantic(id)
      case Neg() => Neg().semantic(id)
      case Zero() => Zero().semantic(id)
      case False() => False().semantic(id)
      case True() => True().semantic(id)
      case MonoDown(baseId) => MonoDown(baseId).semantic(id)
      case MonoUp(baseId) => MonoUp(baseId).semantic(id)
      case GreaterOne() => GreaterOne().semantic(id)
      case LessOne() => LessOne().semantic(id)
      case One() => One().semantic(id)
    }
  }
}



case class HyperTypeCollection(
    val informationFlow: HyperType = High(), 
    val value: Option[HyperType] = None, 
    val mono: Option[MonoTypeCollection] = None,
    val absValue: Option[AbsValueHyperType] = None) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.informationFlow == that.informationFlow && this.value == that.value && this.mono == that.mono && this.absValue == that.absValue
      }
      case _ => false
    }
  }

  def isSubTypeOf(other: HyperTypeCollection): Boolean = {
    val infFlowRes = (this.informationFlow, other.informationFlow) match {
      case (_, High()) => true
      case (Low(),Low()) => true
      case _ => false 
    };
    val valueRes = (this.value, other.value) match {
      case (_, None) => true
      case (Some(Pos()), Some(Pos())) => true
      case (Some(Neg()), Some(Neg())) => true
      case (Some(Zero()), Some(Zero())) => true
      case _ => false 
    };
    val monoRes = (this.mono, other.mono) match {
      case (_, None) => true
      case (Some(mono1), Some(mono2)) => mono1.subsetOf(mono2)
      case _ => false
    }
    val absValue = (this.absValue, other.absValue) match {
      case (_, None) => true
      case (Some(GreaterOne()), Some(GreaterOne())) => true
      case (Some(LessOne()), Some(LessOne())) => true
      case (Some(One()), Some(One())) => true
      case _ => false
    }
    infFlowRes && valueRes && monoRes && absValue
  }

  def joinInfFlow(other: HyperTypeCollection): HyperType = {
    (this.informationFlow, other.informationFlow) match {
      case (Low(), _) => other.informationFlow
      case (_, Low()) => this.informationFlow
      case (High(), _) => High()
      case (_, High()) => High()
      case _ => throw new Exception("This should never happen 1")
    }
  } 

  def joinValue(other: HyperTypeCollection): Option[HyperType] = {
    (this.value, other.value) match {
      case (Some(Pos()), Some(Pos())) => Some(Pos())
      case (Some(Neg()), Some(Neg())) => Some(Neg())
      case (Some(Zero()), Some(Zero())) => Some(Zero())
      case (Some(True()), Some(True())) => Some(True())
      case (Some(False()), Some(False())) => Some(False())
      case _ => None
    }
  }

  def joinAbsValue(other: HyperTypeCollection) : Option[AbsValueHyperType] = {
    (this.absValue, other.absValue) match {
      case (Some(GreaterOne()), Some(GreaterOne())) => Some(GreaterOne())
      case (Some(LessOne()), Some(LessOne())) => Some(LessOne())
      case (Some(One()), Some(One())) => Some(One())
      case _ => None
    }
  }

  def joinMono(other: HyperTypeCollection, pc: HyperType): Option[MonoTypeCollection] = {
    // used to combine traces after i.e. if-else branch

    if (pc == Low() && this == other) {
      // If the program counter is low, we can just return the mono type of this collection
      return this.mono

    } 
    None
  }

  def combineValueOp(other: HyperTypeCollection, op: String) : Option[HyperType] = {
    op match {
      case "+" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Pos())) => Some(Pos())
          case (Some(Neg()), Some(Neg())) => Some(Neg())
          case (Some(Zero()), Some(Zero())) => Some(Zero())
          case (Some(Zero()), Some(Pos())) => Some(Pos())
          case (Some(Pos()), Some(Zero())) => Some(Pos())
          case (Some(Zero()), Some(Neg())) => Some(Neg())
          case (Some(Neg()), Some(Zero())) => Some(Neg())
          case _ => None // This should not happen
        }
      }
      case "-" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Neg())) => Some(Pos())
          case (Some(Neg()), Some(Pos())) => Some(Neg())
          case (Some(Zero()), Some(Zero())) => Some(Zero())
          case (Some(Zero()), Some(Pos())) => Some(Neg())
          case (Some(Zero()), Some(Neg())) => Some(Pos())
          case (Some(Pos()), Some(Zero())) => Some(Pos())
          case (Some(Neg()), Some(Zero())) => Some(Neg())
          case _ => None // This should not happen
        }
      }
      case "*" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Pos())) => Some(Pos())
          case (Some(Neg()), Some(Neg())) => Some(Pos())
          case (Some(Zero()), _) => Some(Zero())
          case (_, Some(Zero())) => Some(Zero())
          case (Some(Neg()), Some(Pos())) => Some(Neg())
          case (Some(Pos()), Some(Neg())) => Some(Neg())
          case _ => None // This should not happen
        }
      }
      case "/" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Pos())) => Some(Pos())
          case (Some(Neg()), Some(Neg())) => Some(Pos())
          case (Some(Zero()), _) => Some(Zero())
          case (_, Some(Zero())) => None // Division by zero is not allowed
          case _ => None // This should not happen
        }
      }
      case "%" => {
        None
      }
      case "&&" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(True()), Some(True())) => Some(True())
          case (Some(False()), _) => Some(False())
          case (_, Some(False())) => Some(False())
          case _ => None // This should not happen
        }
      }
      case "||" => {
        (this.value, other.value) match {
          case (Some(True()), _) => Some(True())
          case (_, Some(True())) => Some(True())
          case (Some(False()), Some(False())) => Some(False())
          case _ => None 
        }
      }
      case "==" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(True()), Some(True())) => Some(True())
          case (Some(False()), Some(False())) => Some(True())
          case (Some(Zero()), Some(Zero())) => Some(True())
          case _ => {
            if (this.absValue == other.absValue && this.value == other.value && (this.absValue == Some(One()))) {
              Some(True())
            } else {
              None
            }
          }
        }
      }
      case "!=" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(True()), Some(False())) => Some(True())
          case (Some(False()), Some(True())) => Some(True())
          case (Some(Zero()), Some(Zero())) => Some(False())
          case (Some(Pos()), Some(Neg())) => Some(True())
          case (Some(Neg()), Some(Pos())) => Some(True())
          case _ => {
            if (this.absValue.isDefined && other.absValue.isDefined && this.value.isDefined && other.value.isDefined) {
                if (this.absValue == other.absValue && this.value == other.value && (this.absValue == Some(One()))) {
                  Some(False())
                } else if (this.absValue != other.absValue) {
                  Some(True())
                } else {
                  None
                }
            } else {
              None
            }
          }
        }
      }
      case "==>" => {
        (this.value, other.value) match {
          case (Some(False()), _) => Some(True())
          case (Some(True()), Some(True())) => Some(True())
          case (Some(True()), Some(False())) => Some(False())
          case _ => None
        }
      }
      case "<" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Neg())) => Some(False())
          case (Some(Neg()), Some(Pos())) => Some(True())
          case (Some(Zero()), Some(Zero())) => Some(False())
          case (Some(Neg()), Some(Zero())) => Some(True())
          case (Some(Zero()), Some(Pos())) => Some(True())
          case (Some(Pos()), Some(Zero())) => Some(False())
          case (Some(Zero()), Some(Neg())) => Some(False())
          case _ => None
        }
      }
      case ">" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Neg())) => Some(True())
          case (Some(Neg()), Some(Pos())) => Some(False())
          case (Some(Zero()), Some(Zero())) => Some(False())
          case (Some(Neg()), Some(Zero())) => Some(False())
          case (Some(Zero()), Some(Pos())) => Some(False())
          case (Some(Pos()), Some(Zero())) => Some(True())
          case (Some(Zero()), Some(Neg())) => Some(True())
          case _ => None // This should not happen
        }
      }
      case "<=" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Neg())) => Some(False())
          case (Some(Neg()), Some(Pos())) => Some(True())
          case (Some(Zero()), Some(Zero())) => Some(True())
          case (Some(Neg()), Some(Zero())) => Some(True())
          case (Some(Zero()), Some(Pos())) => Some(True())
          case (Some(Pos()), Some(Zero())) => Some(False())
          case (Some(Zero()), Some(Neg())) => Some(False())
          case _ => None // This should not happen
        }
      }
      case ">=" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Neg())) => Some(True())
          case (Some(Neg()), Some(Pos())) => Some(False())
          case (Some(Zero()), Some(Zero())) => Some(True())
          case (Some(Neg()), Some(Zero())) => Some(False())
          case (Some(Zero()), Some(Pos())) => Some(False())
          case (Some(Pos()), Some(Zero())) => Some(True())
          case (Some(Zero()), Some(Neg())) => Some(True())
          case _ => None // This should not happen
        }
      }
    }
  }

  def combineMonoOp(other: HyperTypeCollection, op: String) : Option[MonoTypeCollection] = {
    if (this.mono.isEmpty && other.mono.isEmpty) {
      // If both are None, we return None
      return None
    }

    op match {
      case "+" => {
        if (this.mono.nonEmpty && other.informationFlow == Low()){
          // Adding a constant to a mono type, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low()) {
          // Adding a constant to a mono type, gives the same mono type
          other.mono
        } else if (this.mono.nonEmpty && other.mono.nonEmpty) {
          // Adding two mono types, gives the intersection of the two
          this.mono.get.union(other.mono.get)
        } else {
          None
        }
      }
      case "-" => {
        if (this.mono.nonEmpty && other.informationFlow == Low()){
          // Subtracting a constant from a mono type, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low()) {
          // Subtracting a constant from a mono type, gives the oposite monotonicity type
          Some(other.mono.get.flip)
        } else if (this.mono.nonEmpty && other.mono.nonEmpty) {
          this.mono.get.union(other.mono.get.flip)
        }
        else {
          None
        }
      }
      case "*" => {
        if (other.value == Some(Pos()) && other.informationFlow == Low()) {
          this.mono
        } else if (this.value == Some(Pos()) && this.informationFlow == Low()) {
          other.mono
        } else if (other.value == Some(Neg()) && other.informationFlow == Low()) {
          this.mono match {
            case Some(mono) => Some(mono.flip)
            case None => None
          }
        } else if (this.value == Some(Neg()) && this.informationFlow == Low()) {
          other.mono match {
            case Some(mono) => Some(mono.flip)
            case None => None
          }
        } else {
          None
        }
      }
      case "/" => {
        if (other.informationFlow == Low() && other.value == Some(Pos())) {
          // Dividing a monoton type by a positive constant, gives the same mono type
          this.mono
        } else if (this.informationFlow == Low() && this.value == Some(Neg())) {
          // Dividing a negative constant by a mono type, gives the oposite monotonicity type
          other.mono match {
            case Some(mono) => Some(mono.flip)
            case None => None
          }
        } else {
          None
        }
      }
      case "<" | ">" | "<=" | ">=" => { 
        val normalized = other.combineMonoOp(this, "-"); // other - this this means: a < b becoms 0 < b - a 
        if (op == "<" || op == "<=") {
          // If we are checking if this is less than other, we can just return the normalized type
          return normalized
        } else {
          // If we are checking if this is greater than other, we need to flip the monotonicity type
          normalized match {
            case Some(mono) => Some(mono.flip)
            case None => None
          }
        }
      }
      case "==" | "!=" | "&&" | "||" | "==>" => {
        None
      }
      case _ => None
    // TODO: "Not" also just inverts it. And &&, || etc. may also be possible.
    }
  }

  def combineAbsValueOp(other: HyperTypeCollection, op: String) : Option[AbsValueHyperType] = {
    op match {
      case "+" => {
        if (this.value == Some(Zero())) {
          return other.absValue
        } else if (other.value == Some(Zero())) {
          return this.absValue
        }

        (this.absValue, other.absValue) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(GreaterOne()), _) => {
            if (this.value.isDefined && this.value == other.value) {
              // They are adding in the same direction
              Some(GreaterOne())
            } else None
          }
          case (Some(LessOne()), Some(LessOne())) => {
            if (this.value.isDefined && other.value.isDefined && this.value != other.value) {
              // they have opposite signs
              Some(LessOne())
            } else {
              None
            }
          }
          case (Some(One()), Some(One())) => {
            if (this.value.isDefined && other.value.isDefined && this.value != other.value) {
              // they ahve opposite signs
              Some(LessOne())
            } else None
          }
          case _ => None // This should not happen
        }
      }
      case "-" => {
        if (this.value == Some(Zero())) {
          return other.absValue
        } else if (other.value == Some(Zero())) {
          return this.absValue
        }

        (this.absValue, other.absValue) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(GreaterOne()), Some(GreaterOne())) => {
            if (this.value.isDefined && other.value.isDefined && this.value != other.value) {
              // They are in opposite directions, but the minus flips them into the same direction
              Some(GreaterOne())
            } else None
          }
          case (Some(LessOne()), Some(LessOne())) => {
            if (this.value.isDefined && other.value.isDefined && this.value == other.value) {
              // they have equal sign but the minus flips one of them.
              Some(LessOne())
            } else {
              None
            }
          }
          case (Some(One()), Some(One())) => {
            if (this.value.isDefined && other.value.isDefined && this.value == other.value) {
              // they have equal sign but the minus flips one of them.
              Some(LessOne())
            } else None
          }
          case _ => None
        }
      }
      case "*" => {
        if (this.value == Some(Zero()) || other.value == Some(Zero())) {
          return Some(LessOne())
        }

        (this.absValue, other.absValue) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(One()), _) => other.absValue
          case (_, Some(One())) => this.absValue
          case (Some(GreaterOne()), Some(GreaterOne())) => Some(GreaterOne())
          case (Some(LessOne()), Some(LessOne())) => Some(LessOne())
        }
      }
      case "/" => {
        if (this.value == Some(Zero())) {
          return Some(LessOne())
        }
        if (other.value == Some(Zero())) {
          throw new Exception("Division by zero is not allowed")
        }

        (this.absValue, other.absValue) match {
          case (None, _) => None
          case (_, None) => None
          case (_, Some(One())) => this.absValue
          case (Some(GreaterOne()), Some(LessOne())) => Some(GreaterOne())
          case (Some(One()), Some(LessOne())) => Some(GreaterOne())
          case (Some(One()), Some(GreaterOne())) => Some(LessOne())
          case (Some(LessOne()), Some(GreaterOne())) => Some(LessOne())
          case _ => None 
        }
      }
      case _ => None
    }

  }


  // print
  override def toString: String = {
    PrettyPrinter.formatHyperTypeCollection(this)
  }

  def deepcopy() : HyperTypeCollection = {
    new HyperTypeCollection(informationFlow)
  }

  def isEmpty() : Boolean = {
    this.informationFlow == High() && this.value.isEmpty && this.mono.isEmpty
  }
}



object HyperTypeCollection {

 def fromSeq(seq: Seq[HyperType]): HyperTypeCollection = {
    var infFlowType : HyperType = High();
    var valueType : Option[HyperType] = None;
    var monoType : Option[MonoHyperType] = None;
    var absValueType : Option[AbsValueHyperType] = None;
    for (ty <- seq) {
      ty match {
        case Low()  => {
          infFlowType = Low()
        }
        case Pos() | Neg() | Zero() | True() | False() => {
          valueType match {
            case None => valueType = Some(ty)
            case Some(v) => { throw new Exception("Cannot combine value types " + v + " and " + ty) }
          }
        } 
        case MonoUp(_) | MonoDown(_) => { 
          monoType match {
            case None => monoType = Some(ty.asInstanceOf[MonoHyperType])
            case Some(_) => throw new Exception("Cannot combine mono types " + monoType + " and " + ty)
          }
        }
        case GreaterOne() | One() | LessOne() => {
          monoType match {
            case None => absValueType = Some(ty.asInstanceOf[AbsValueHyperType])
            case Some(_) => throw new Exception("Cannot combine abs value types " + absValueType + " and " + ty)
          }
        } 
        case _ => {
          throw new Exception("Unknown hyper type " + ty)
        }
      }
    }

    val mono = monoType match {
      case Some(mono) => Some(MonoTypeCollection(mono))
      case None => None
    }

    new HyperTypeCollection(informationFlow = infFlowType, value = valueType, mono = mono, absValue = absValueType)
  }
}
