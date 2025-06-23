package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.management.PrettyPrinter

case class DeltaCollection(val collection: Map[String,DeltaMapping]) {


    def combine(other: DeltaCollection): DeltaCollection = {
        val keys = this.collection.keySet.intersect(other.collection.keySet)
        val newMapping = keys.map { key =>
            val xType = this.collection.get(key)
            val yType = other.collection.get(key)
            (xType, yType) match {
                case (Some(x), Some(y)) => {
                    key -> x.combine(y)
                }
                case _ => {
                    throw new NoSuchElementException(s"Key $key not found in some mapping")
                }
            }
        }.toMap

        new DeltaCollection(newMapping)
    }; 
    override def equals(other: Any): Boolean = {
        other match {
            case that: DeltaCollection => {
                if (this.collection.size != that.collection.size) {
                    return false
                }
                this.collection.forall { case (key, value) =>
                    that.collection.get(key) match {
                        case Some(otherValue) => value == otherValue
                        case None => false
                    }
                }
            }
            case _ => false
        }
    }

    override def toString(): String = {
        PrettyPrinter.formatDeltaCollection(this)
    }
}

case class DeltaMapping(val mapping: Map[String, HyperTypeCollection]) {

    def flipSign(): DeltaMapping = {
        val newMapping = mapping.map { case (key, value) =>
            value.value match {
                case Some(Pos()) => (key, HyperTypeCollection(value.informationFlow, Some(Neg())))
                case Some(Neg()) => (key, HyperTypeCollection(value.informationFlow, Some(Pos())))
                case Some(Zero()) => (key, HyperTypeCollection(value.informationFlow, Some(Zero())))
                case _ => (key, value)
            }
        }
        new DeltaMapping(newMapping)
    }

    def combine(other: DeltaMapping) : DeltaMapping = {
        val keys = this.mapping.keySet.intersect(other.mapping.keySet)

        val newMapping = keys.map { key =>
            val xType = this.mapping.get(key)
            val yType = other.mapping.get(key)
            (xType, yType) match {
                case (Some(x), Some(y)) => {
                    val value = x.joinValue(y);
                    val infFlow = value match {
                        case Some(True()) | Some(False()) | Some(Zero()) => {
                            Low()
                        }
                        case _ => High()
                    }

                    key -> HyperTypeCollection(informationFlow = infFlow, value = value);
                }
                case _ => {
                    throw new NoSuchElementException(s"Key $key not found some mapping")
                }
            }
        }.toMap

        new DeltaMapping(newMapping)
    }

    override def toString(): String = {
        PrettyPrinter.formatDeltaMapping(this)
    }

    override def equals(other: Any) : Boolean = {
        other match {
            case that: DeltaMapping => {
                if (this.mapping.size != that.mapping.size) {
                    return false
                }
                this.mapping.forall { case (key, value) =>
                    that.mapping.get(key) match {
                        case Some(otherValue) => value == otherValue
                        case None => false
                    }
                }
            }
            case _ => false
        }
    }
}


object DeltaMapping {


    def combineOp(typeLeft : HyperTypeCollection, deltaLeft: DeltaMapping, typeRight: HyperTypeCollection, deltaRight  : DeltaMapping, op: String): DeltaMapping = {
        val keysLeft = deltaLeft.mapping.keySet
        val keysRight = deltaRight.mapping.keySet
        val keysBoth = keysLeft.intersect(keysRight)


        val keysLeftOnly = keysLeft.diff(keysBoth)
        val keysRightOnly = keysRight.diff(keysBoth)
        var newMapping = Map[String, HyperTypeCollection]()
        keysLeftOnly.foreach(key => {
            val deltaType = deltaLeft.mapping(key);
            var infFlowType = deltaType.joinInfFlow(typeRight);
            val valueType = deltaType.combineValueOp(typeRight, op);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                    infFlowType = Low()
                }
                case _ => {}
            }
            newMapping += (key -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
        });

        keysRightOnly.foreach(key => {
            val deltaType = deltaRight.mapping(key);
            var infFlowType = typeLeft.joinInfFlow(deltaType);
            val valueType = typeLeft.combineValueOp(deltaType, op);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                    infFlowType = Low()
                }
                case _ => {}
            }
            newMapping += (key -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
        });

        keysBoth.foreach(key => {
            val deltaTypeLeft = deltaLeft.mapping(key);
            val deltaTypeRight = deltaRight.mapping(key);
            var infFlowType = deltaTypeLeft.joinInfFlow(deltaTypeRight);
            val valueType = deltaTypeLeft.combineValueOp(deltaTypeRight, op);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                    infFlowType = Low()
                }
                case _ => {}
            }
            newMapping += (key -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
        });
        new DeltaMapping(newMapping)
    }


    def composite(before : Map[String, DeltaMapping], after : Map[String, DeltaMapping]): Map[String, DeltaMapping] = {
        val keysBefore = before.keySet
        val keysAfter = after.keySet
        val keysBoth = keysBefore.intersect(keysAfter)

        val keysBeforeOnly = keysBefore.diff(keysBoth)

        var newMapping = Map[String, DeltaMapping]()
        keysBeforeOnly.foreach(key => {
            newMapping += (key -> before(key))
        });
        keysAfter.foreach(key => {
            val dependency = after(key)
            var dependencies = Map[String, HyperTypeCollection]()

            dependency.mapping.foreach({ case (depKey, depValue) =>
               if (before.contains(depKey)) {
                    val beforeValue = before(depKey)
                    for ((beforeKey, beforeValueType) <- beforeValue.mapping) {
                        var infFlowType = beforeValueType.joinInfFlow(depValue);
                        var valueType = beforeValueType.joinValue(depValue);

                        if (dependencies.contains(beforeKey)) {
                            val existingDependency = dependencies(beforeKey)
                            val collectionBefore = new HyperTypeCollection(infFlowType, valueType)
                            infFlowType = existingDependency.joinInfFlow(collectionBefore)
                            valueType = existingDependency.joinValue(collectionBefore)
                        }
                        valueType match {
                            case Some(True()) | Some(False()) | Some(Zero()) => {
                                dependencies += (beforeKey -> HyperTypeCollection(informationFlow = Low(), value = valueType))
                            }
                            case _ => {
                                dependencies += (beforeKey -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
                            }
                        }
                    }

               } else {
                    dependencies += (depKey -> depValue)
               }
            });

            newMapping += (key -> after(key))
        });
        newMapping
    }
}