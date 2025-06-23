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

case class MonoUp(val id : Id) extends HyperType {
  // TODO
  def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}
case class MonoDown(val id : Id) extends HyperType {
  // TODO
  def semantic_vpr() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    (None, Seq.empty)
  }
}



case class HyperTypeCollection(var informationFlow: HyperType = High(), var value: Option[HyperType] = None, var mono: Option[HyperType] = None) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.informationFlow == that.informationFlow && this.value == that.value && this.mono == that.mono
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
      case (Some(MonoUp(left)), Some(MonoUp(right))) => left == right
      case (Some(MonoDown(left)), Some(MonoDown(right))) => left == right
      case _ => false
    }
    infFlowRes && valueRes && monoRes
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

  def joinMono(other: HyperTypeCollection, pc: HyperType): Option[HyperType] = {
    (this.mono, other.mono) match{
      case (None, _) => None
      case (_, None) => None
      case (Some(MonoUp(left)), Some(MonoUp(right))) => {
        if (left == right && pc == Low()) {
          Some(MonoUp(left))
        } else {
          None
        }
      }
      case (Some(MonoDown(left)), Some(MonoDown(right))) => {
        if (left == right && pc == Low()) {
          Some(MonoDown(left))
        } else {
          None
        }
      }
      case _ => None
    }
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

  def combineMonoOp(other: HyperTypeCollection, op: String) : Option[HyperType] = {
    op match {
      case "+" => {
        if (this.mono == other.mono) {
          // Both are the same or both are None
          this.mono
        } else if (this.mono.nonEmpty && other.informationFlow == Low()){
          // Adding a constant to a mono type, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low()) {
          // Adding a constant to a mono type, gives the same mono type
          other.mono
        } else {
          None // This should not happen
        }
      }
      case "-" => {
        if (this.mono == other.mono) {
          // Both are the same or both are None. Since we are subtracting, we do not know what happens
          None
        } else if (this.mono.nonEmpty && other.informationFlow == Low()){
          // Subtracting a constant from a mono type, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low()) {
          // Subtracting a constant from a mono type, gives the oposite monotonicity type
          other.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else {
          None // This should not happen
        }
      }
      case "*" => {
        if (this.mono == this.mono && this.value == Some(Pos()) && other.value == Some(Pos())) {
          // Since the only positive values are considered, the monotonicity type remains the same.
          this.mono
        } else if (this.mono == this.mono && this.value == Some(Neg()) && other.value == Some(Neg())) {
          // Since both are negative, the monotonicty is inverted
          this.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else if (this.mono.nonEmpty && other.informationFlow == Low() && other.value == Some(Pos())) {
          // Multiplying a constant to a mono type, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low() && this.value == Some(Pos())) {
          // Multiplying a constant to a mono type, gives the same mono type
          other.mono
        } else if (this.mono.nonEmpty && other.informationFlow == Low() && other.value == Some(Neg())) {
          // Multiplying a constant to a mono type, gives the oposite monotonicity type
          this.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else if (other.mono.nonEmpty && this.informationFlow == Low() && this.value == Some(Neg())) {
          // Multiplying a constant to a mono type, gives the oposite monotonicity type
          other.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else {
          None
        }
      }
      case "/" => {
        if (this.mono.nonEmpty && other.informationFlow == Low() && other.value == Some(Pos())) {
          // Dividing a monoton type by a positive constant, gives the same mono type
          this.mono
        } else if (other.mono.nonEmpty && this.informationFlow == Low() && this.value == Some(Pos())) {
          // Dividing a constant by a mono type, flipps the monotonicity type
          other.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else if (this.mono.nonEmpty && other.informationFlow == Low() && other.value == Some(Neg())) {
          // Dividing a monotone type by a negative constant, gives the oposite monotonicity type
          this.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else if (other.mono.nonEmpty && this.informationFlow == Low() && this.value == Some(Neg())) {
          // Dividing a negative constant by a mono type, gives the oposite monotonicity type
          other.mono match {
            case Some(MonoUp(id)) => Some(MonoDown(id))
            case Some(MonoDown(id)) => Some(MonoUp(id))
            case None => throw new Exception("This should not happen")
          }
        } else {
          None // This should not happen
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
 
}

object HyperTypeCollection {

 def fromSeq(seq: Seq[HyperType]): HyperTypeCollection = {
    var infFlowType : HyperType = High();
    var valueType : Option[HyperType] = None;
    var monoType : Option[HyperType] = None;
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
            case None => monoType = Some(ty)
            case Some(m) => { throw new Exception("Cannot combine monotonicity types " + m + " and " + ty) }
          }
        }
        case _ => {
          throw new Exception("Unknown hyper type " + ty)
        }
      }
    }
    new HyperTypeCollection(informationFlow = infFlowType, value = valueType)
  }
}
