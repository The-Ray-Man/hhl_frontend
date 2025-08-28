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
import viper.HHLVerifier.typing.dsl.RuleCheckContext
import viper.HHLVerifier.typing.dsl.HyperType

object HyperTypeCollection {

  def fromSeq(seq: Seq[HyperType]): HyperTypeCollection = {

    new HyperTypeCollection(seq.toSet)
  }
}

/** `HyperTypeCollection` is a wrapper for a set of hypertypes */
case class HyperTypeCollection(
    val hypertypes: Set[HyperType] = Set.empty[HyperType]
) {
  override def equals(obj: Any): Boolean = {
    obj match {
      case that: HyperTypeCollection => {
        this.hypertypes == that.hypertypes
      }
      case _ => false
    }
  }

  /** returns true if and only if `this` is a subset of `other` */
  def isSubTypeOf(other: HyperTypeCollection): Boolean = {
    this.hypertypes.forall(other.hypertypes.contains)
  }

  /** Creates a new `HyperTypeCollection`, where `ty` is added to `this` */
  def add(ty: HyperType): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes + ty)
  }

  /** Creates a new `HyperTypeCollection`, where all the types in `other` are added to `this` */
  def extend(other: HyperTypeCollection): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes ++ other.hypertypes)
  }

  /** Creates a new `HyperTypeCollection`, where `ty` is removed (if it exists) from `this` */
  def without(ty: HyperType): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes - ty)
  }

  override def toString: String = {
    PrettyPrinter.formatHyperTypeCollection(this)
  }
}

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

  def get(key: String): HyperTypeCollection = {
    mapping.get(key) match {
      case Some(value) => value
      case None        => HyperTypeCollection(Set())
    }
  }

  override def toString: String = {
    PrettyPrinter.formatHyperTypeMapping(this)
  }
}

/** A `DeltaCollection` maps variable names to the delta collection. It is used to track the dependencies within an expression.
  */
case class DeltaCollection(val mapping: Map[String, HyperTypeCollection]) {

  override def toString(): String = {
    PrettyPrinter.formatDeltaCollection(this)
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: DeltaCollection => {
        if (this.mapping.size != that.mapping.size) {
          return false
        }
        this.mapping.forall { case (key, value) =>
          that.mapping.get(key) match {
            case Some(otherValue) => value == otherValue
            case None             => false
          }
        }
      }
      case _ => false
    }
  }
  def isEmpty: Boolean = {
    mapping.isEmpty
  }

  /** Adds a hyperType to the dataCollection. If `variable` already exists in the mapping, the `hyperType` is added to the existing collection. Otherwise, a new collection is created.
    *
    * @param variable
    *   The name of the variable to which the hypertype should be added.
    * @param hyperType
    *   The hypertype to add.
    * @return
    *   The new `DeltaCollection`
    */
  def add(variable: String, hyperType: dsl.HyperType): DeltaCollection = {
    val updatedMapping = mapping.get(variable) match {
      case Some(existingCollection) => mapping.updated(variable, existingCollection.add(hyperType))
      case None                     => mapping.updated(variable, HyperTypeCollection(Set(hyperType)))
    }
    DeltaCollection(updatedMapping)
  }

  /** Extends the collection for a variable with a set of hypertypes. If `variable` already exists in the mapping, the `hyperTypes` are added to the existing collection. Otherwise, a new collection is created.
    *
    * @param variable
    *   The name of the variable to extend.
    * @param hyperTypes
    *   The hypertypes to add.
    * @return
    *   The new `DeltaCollection`
    */

  def extend(variable: String, hyperTypes: HyperTypeCollection): DeltaCollection = {
    val updatedMapping = mapping.get(variable) match {
      case Some(existingCollection) => mapping.updated(variable, existingCollection.extend(hyperTypes))
      case None                     => mapping.updated(variable, hyperTypes)
    }
    DeltaCollection(updatedMapping)
  }

}

/** A `DeltaMapping` maps variable names to the delta collection. It is used to track the delta types across statements. Example: Assume we have the mapping x -> {y -> LOW, z -> POS}, this could mean that the difference between the current `x` and the initial `y` is the same for all traces, and the difference between the current `x` and the inital `z` is positive for all traces.
  */
case class DeltaMapping(val collection: Map[String, DeltaCollection]) {

  override def equals(other: Any): Boolean = {
    other match {
      case that: DeltaMapping => {
        if (this.collection.size != that.collection.size) {
          return false
        }
        this.collection.forall { case (key, value) =>
          that.collection.get(key) match {
            case Some(otherValue) => value == otherValue
            case None             => false
          }
        }
      }
      case _ => false
    }
  }

  override def toString(): String = {
    PrettyPrinter.formatDeltaMapping(this)
  }

  def isEmpty: Boolean = {
    collection.isEmpty
  }
}
