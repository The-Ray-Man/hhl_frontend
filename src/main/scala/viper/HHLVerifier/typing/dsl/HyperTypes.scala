package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.{Id, Expr}
import scala.collection.immutable.{Set => ScalaSet}

trait Element extends CollectVariables {}

abstract class HyperType extends Element {
  override def equals(obj: Any): Boolean
  def semantics(id: Id): Expr = throw new Exception("Semantics not defined for HyperType: " + this.getClass.getSimpleName)
}

case class SimpleHyperType(name: String) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case SimpleHyperType(otherName) => name == otherName
    case _                          => false
  }

  override def variables: ScalaSet[Id] = ScalaSet.empty[Id]
}
case class HyperTypeWithSetArgs(name: SimpleHyperType, args: ScalaSet[Element]) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithSetArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                          => false
  }

  override def variables: ScalaSet[Id] = args.flatMap(_.variables)
}
case class HyperTypeWithListArgs(name: SimpleHyperType, args: Seq[Element]) extends HyperType {

  override def equals(obj: Any): Boolean = obj match {
    case HyperTypeWithListArgs(otherName, otherArgs) => name == otherName && args == otherArgs
    case _                                           => false
  }

  override def variables: ScalaSet[Id] = args.flatMap(_.variables).toSet
}
