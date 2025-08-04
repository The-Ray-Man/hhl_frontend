package viper.HHLVerifier.typing

import viper.HHLVerifier.management.PrettyPrinter

case class HyperMapping(val mapping: Map[String, HyperTypeCollection]) {
  override def equals(other: Any): Boolean = {
    other match {
      case that: HyperMapping =>
        this.mapping == that.mapping
      case _ => false
    }
  }

  def set(key: String, value: HyperTypeCollection): HyperMapping = {
    val newMapping = mapping + (key -> value)
    val result     = new HyperMapping(newMapping)
    result
  }

  def get(key: String): Option[HyperTypeCollection] = {
    // mapping.get(key)
    mapping.get(key)
  }

  def getUnsafe(key: String): HyperTypeCollection = {
    mapping.get(key) match {
      case Some(value) => value
      case None        => throw new NoSuchElementException(s"Key $key not found in mapping")
    }
  }

  override def toString: String = {
    PrettyPrinter.formatHyperTypeMapping(this)
  }
}
