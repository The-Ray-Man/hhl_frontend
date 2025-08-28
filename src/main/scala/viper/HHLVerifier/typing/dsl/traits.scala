package viper.HHLVerifier.typing.dsl

import viper.HHLVerifier.ast.Id
import scala.collection.immutable.{Set => ScalaSet}

trait CollectVariables {
  def variables: ScalaSet[Id]
}
