package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter
import viper.HHLVerifier.typing.HyperLattice


sealed trait HyperType {
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }

  // def join(other: HyperType): HyperType = {
  //   HyperLattice.join(this, other)
  // }
  // def meet(other: HyperType): HyperType = {
  //   HyperLattice.meet(this, other)
  // }  
}

case class Low() extends HyperType 
case class High() extends HyperType


class HyperTypeCollection(var informationFlow: Option[HyperType] = None) {

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
