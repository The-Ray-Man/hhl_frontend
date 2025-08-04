package viper.HHLVerifier.typing.dsl

object Mappings {
  def mapSpecification(derivationRules: Seq[DerivationRule]): Specification = {
    // Implementation to convert a sequence of derivation rules into a Specification
    Specification(derivationRules) // Placeholder, replace with actual mapping logic
  }

  def mapMappingAccess(collection: Mapping, id: String): MappingAccess = MappingAccess(collection, id)
}
