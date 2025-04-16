package viper.HHLVerifier.typing

import viper.HHLVerifier.management.UnknownException
import viper.HHLVerifier.ast.Id

// trait for a finite lattice
trait Lattice[T] {
    def join(x: T, y: T): T
    def meet(x: T, y : T): T
}

trait FiniteLattice[T] extends Lattice[T] {
    def minimum(): T
    def maximum(): T
}


object HyperLattice extends FiniteLattice[HyperType] with PartialOrdering[HyperType] {

  override def tryCompare(x: HyperType, y: HyperType): Option[Int] = {
    (x, y) match {
        case (Low(), Low()) => Some(0)
        case (Low(), _) => Some(-1)
        case (_, Low()) => Some(1)
        case (High(), High()) => Some(0)
        case _ => None
    }
  }

  override def lteq(x: HyperType, y: HyperType): Boolean = {
    tryCompare(x, y) match {
       case Some(x) => x <= 0
        case None => false
    }
  }


  override def minimum(): HyperType = Low()

  override def maximum(): HyperType = High()


  override def join(x: HyperType, y: HyperType): HyperType = {
    (x, y) match {
        case (Low(), _) => y
        case (_, Low()) => x
        case (High(), _) => High()
        case _ => throw UnknownException("Not implemented")
    }
  }

  override def meet(x: HyperType, y: HyperType): HyperType = {
    (x, y) match {
        case (Low(), _) => Low()
        case (_, Low()) => Low()
        case (High(), _) => x
        case _ => throw UnknownException("Not implemented")
    }
  }
}



class HyperMapping extends Lattice[HyperMapping] {

  
  var mapping : Map[String, HyperType] = Map()

  override def join(x: HyperMapping, y: HyperMapping): HyperMapping = {
    val keys = x.mapping.keySet ++ y.mapping.keySet
    val newMapping = keys.map { key =>
      val xType = x.mapping.getOrElse(key, HyperLattice.minimum())
      val yType = y.mapping.getOrElse(key, HyperLattice.minimum())
      key -> HyperLattice.join(xType, yType)
    }.toMap
    val result = new HyperMapping()
    result.mapping = newMapping
    result
  }

  override def meet(x: HyperMapping, y: HyperMapping): HyperMapping = {
    val keys = x.mapping.keySet ++ y.mapping.keySet
    val newMapping = keys.map { key =>
      val xType = x.mapping.getOrElse(key, HyperLattice.maximum())
      val yType = y.mapping.getOrElse(key, HyperLattice.maximum())
      key -> HyperLattice.meet(xType, yType)
    }.toMap
    val result = new HyperMapping()
    result.mapping = newMapping
    result
  }

  def meet(other: HyperMapping): HyperMapping = {
    meet(this, other)
  }

  def join(other: HyperMapping): HyperMapping = {
    join(this, other)
  }

  def set(key: String, value: HyperType): Unit = {
    mapping = mapping + (key -> value)
  }

  def get(key: String): Option[HyperType] = {
    mapping.get(key)
  }

  def getUnsafe(key: String): HyperType = {
    mapping.get(key) match {
      case Some(value) => value
      case None => throw new NoSuchElementException(s"Key $key not found in mapping")
    }
  }

}