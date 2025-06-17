package viper.HHLVerifier.typing



case class HyperMapping(val mapping : Map[String, HyperTypeCollection]) {
  override def equals(other: Any): Boolean = {
    other match {
      case that: HyperMapping => 
        this.mapping == that.mapping
      case _ => false
    }
  }

  def combine(other: HyperMapping): HyperMapping = {
    val keys = this.mapping.keySet ++ other.mapping.keySet

    val newMapping = keys.map { key =>
      val xType = this.mapping.get(key)
      val yType = other.mapping.get(key)
      (xType, yType) match {
        case (Some(x), Some(y)) => {
          val infFlow = x.joinInfFlow(y);
          val value = x.joinValue(y);
          key -> HyperTypeCollection(informationFlow = infFlow, value = value);
        }
        case _ => {
          key -> HyperTypeCollection(informationFlow = Some(High()), value = None)
        }
      }
    }.toMap

    val result = new HyperMapping(newMapping)
    result
  }

  def set(key: String, value: HyperTypeCollection): HyperMapping = {
    val newMapping = mapping + (key -> value)
    val result = new HyperMapping(newMapping)
    result
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