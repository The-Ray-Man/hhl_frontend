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


object HyperLattice extends FiniteLattice[HyperTypeCollection] with PartialOrdering[HyperTypeCollection] {

  override def tryCompare(x: HyperTypeCollection, y: HyperTypeCollection): Option[Int] = {
    // Compare two sets of hyper types
    (x.informationFlow, y.informationFlow) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(Low()), Some(Low())) => Some(0)
      case (Some(Low()), Some(High())) => Some(-1)
      case (Some(High()), Some(Low())) => Some(1)
      case (Some(High()), Some(High())) => Some(0)
    }

  }

  override def lteq(x: HyperTypeCollection, y: HyperTypeCollection): Boolean = {
    tryCompare(x, y) match {
       case Some(x) => x <= 0
        case None => false
    }
  }


  override def minimum(): HyperTypeCollection = new HyperTypeCollection(Some(Low()))

  override def maximum(): HyperTypeCollection = new HyperTypeCollection(Some(High()))


  override def join(x: HyperTypeCollection, y: HyperTypeCollection): HyperTypeCollection = {
    (x.informationFlow, y.informationFlow) match {
        case (None, _) => new HyperTypeCollection(Some(High()))
        case (_, None) => new HyperTypeCollection(Some(High()))
        case (Some(Low()), _) => y
        case (_, Some(Low())) => x
        case (Some(High()), _) => new HyperTypeCollection(Some(High()))
        case _ => throw UnknownException("Not implemented")
    }
  }

  override def meet(x: HyperTypeCollection, y: HyperTypeCollection): HyperTypeCollection = {
      (x.informationFlow, y.informationFlow) match {
        case (None, _) => new HyperTypeCollection(Some(Low()))
        case (_, None) => new HyperTypeCollection(Some(Low()))
        case (Some(Low()), _) => x
        case (_, Some(Low())) => y
        case (Some(High()), Some(High())) => new HyperTypeCollection(Some(High()))
        case _ => throw UnknownException("Not implemented")
    }
  }
}





class HyperMapping extends Lattice[HyperMapping]  {

  
  var mapping : Map[String, HyperTypeCollection] = Map()

  override def equals(other: Any): Boolean = {
    other match {
      case that: HyperMapping => 
        this.mapping == that.mapping
      case _ => false
    }
  }

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

  def set(key: String, value: HyperTypeCollection): Unit = {
    mapping = mapping + (key -> value)
  }

  def get(key: String): Option[HyperTypeCollection] = {
    // mapping.get(key)
    mapping.get(key)
  }

  def getUnsafe(key: String): HyperTypeCollection = {
    mapping.get(key) match {
      case Some(value) => value
      case None => throw new NoSuchElementException(s"Key $key not found in mapping")
    }
  }

}