package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter

sealed trait HyperType{
  override def toString: String = {
    PrettyPrinter.formatHyperType(this)
  }
}

case class Low() extends HyperType
case class High() extends HyperType