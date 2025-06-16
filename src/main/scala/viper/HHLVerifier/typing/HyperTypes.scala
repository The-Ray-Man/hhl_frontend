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

sealed trait HyperType {
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }
}

case class Low() extends HyperType {


  def semantic(id: Id) : Assertion = {
    val s1_assert = AssertVar("_s1")
    val s2_assert = AssertVar("_s2")
    val s1 = AssertVarDecl(s1_assert, StateType())
    val s2 = AssertVarDecl(s2_assert, StateType())
    val stmt = Assertion("forall", Seq(s1,s2), BinaryExpr(BinaryExpr(StateExistsExpr(s1_assert, false), "&&", StateExistsExpr(s2_assert, false)) ,"==>",BinaryExpr(LookupExpr(s1_assert, id),"==",LookupExpr(s2_assert, id)) ))
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

  def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    // High does not have a semantic, it is just a placeholder
    (None, Seq.empty)
  }

}

case class Pos() extends HyperType {
  // TODO
  def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}

case class Neg() extends HyperType {
  // TODO
    def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}

case class Zero() extends HyperType {
  // TODO
    def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}

case class True() extends HyperType {
  // TODO
    def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}

case class False() extends HyperType {
  // TODO
    def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}

case class HyperTypeCollection(var informationFlow: Option[HyperType] = None, var value: Option[HyperType] = None) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.informationFlow == that.informationFlow && this.value == that.value
      }
      case _ => false
    }
  }

  def isSubTypeOf(other: HyperTypeCollection): Boolean = {
    val infFlowRes = (this.informationFlow, other.informationFlow) match {
      case (_, None) => true
      case (_, Some(High())) => true
      case (Some(Low()), Some(Low())) => true
      case _ => false 
    };
    val valueRes = (this.value, other.value) match {
      case (_, None) => true
      case (Some(Pos()), Some(Pos())) => true
      case (Some(Neg()), Some(Neg())) => true
      case (Some(Zero()), Some(Zero())) => true
      case _ => false 
    };
    infFlowRes && valueRes
  }

  def joinInfFlow(other: HyperTypeCollection): Option[HyperType] = {
    (this.informationFlow, other.informationFlow) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(Low()), _) => other.informationFlow
      case (_, Some(Low())) => this.informationFlow
      case (Some(High()), _) => Some(High())
      case (_, Some(High())) => Some(High())
      case _ => None // This should not happen
    }
  } 

  def joinValue(other: HyperTypeCollection): Option[HyperType] = {
    (this.value, other.value) match {
      case (Some(Pos()), Some(Pos())) => Some(Pos())
      case (Some(Neg()), Some(Neg())) => Some(Neg())
      case (Some(Zero()), Some(Zero())) => Some(Zero())
      case _ => None 
    }
  }

  def joinValue(other: HyperTypeCollection, op: String) : Option[HyperType] = {
    op match {
      case "+" => {
        (this.value, other.value) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(Pos()), Some(Pos())) => Some(Pos())
          case (Some(Neg()), Some(Neg())) => Some(Neg())
          case (Some(Zero()), Some(Zero())) => Some(Zero())
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
          case _ => None
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
          case _ => None
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
          case _ => None // This should not happen
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

  // print
  override def toString: String = {
    PrettyPrinter.formatHyperTypeCollection(this)
  }

  def deepcopy() : HyperTypeCollection = {
    new HyperTypeCollection(informationFlow)
  }

  def is_empty() : Boolean = {
    informationFlow.isEmpty
  }
 
}

object HyperTypeCollection {

 def fromSeq(seq: Seq[HyperType]): HyperTypeCollection = {
    val collection = new HyperTypeCollection();
    for (ty <- seq) {
      ty match {
        case Low() | High() => {
          if (collection.informationFlow.isEmpty) {
            collection.informationFlow = Some(ty)
          } else {
            throw new Exception("Cannot have both low and high information flow")
          }
        }
        case Pos() | Neg() | Zero() => {
          if (collection.value.isEmpty) {
            collection.value = Some(ty)
          } else {
            throw new Exception("Cannot have both positive and negative sign")
          }
        } 
        case _ => {
          throw new Exception("Unknown hyper type " + ty)
        }
      }
    }
    collection
  }
}
