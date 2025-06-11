package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter
import viper.HHLVerifier.ast.Assertion
import viper.HHLVerifier.generation.State
import viper.silver.{ast => vpr}
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.generation.SetState

sealed trait HyperType {
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }
}

case class Low() extends HyperType {

  def semantic(id : Id, s0VarName: String, s1VarName:String, STmp : vpr.LocalVar) : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
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

  def semantic() : (Option[vpr.Exp], Seq[vpr.LocalVar]) = {
    // High does not have a semantic, it is just a placeholder
    (None, Seq.empty)
  }

}


case class HyperTypeCollection(var informationFlow: Option[HyperType] = None) {

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.informationFlow == that.informationFlow
      }
      case _ => false
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
        case _ => {
          throw new Exception("Unknown hyper type " + ty)
        }
      }
    }
    collection
  }
}
