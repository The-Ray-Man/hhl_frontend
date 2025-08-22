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

  def isSubTypeOf(other: HyperTypeCollection): Boolean = {
    this.hypertypes.forall(other.hypertypes.contains)
  }

  def add(ty: HyperType): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes + ty)
  }

  def extend(other: HyperTypeCollection): HyperTypeCollection = {
    new HyperTypeCollection(this.hypertypes ++ other.hypertypes)
  }

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
