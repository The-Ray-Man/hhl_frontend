package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter
import viper.HHLVerifier.typing.HyperLattice


sealed trait HyperType {
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }

  def join(other: HyperType): HyperType = {
    HyperLattice.join(this, other)
  }
  def meet(other: HyperType): HyperType = {
    HyperLattice.meet(this, other)
  }  
}

case class Low() extends HyperType 
case class High() extends HyperType


