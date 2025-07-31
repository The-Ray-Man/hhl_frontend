package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.management.PrettyPrinter

case class DeltaCollection(val collection: Map[String,DeltaMapping]) {


    def combine(other: DeltaCollection, condType : HyperTypeCollection): DeltaCollection = {
        // Combines two DeltaCollection. This should be used to combine two delta collections from two different paths in the program. (like if-else branches)
        val keys = this.collection.keySet.intersect(other.collection.keySet)
        val newMapping = keys.map { key =>
            val xType = this.collection.get(key)
            val yType = other.collection.get(key)
            (xType, yType) match {
                case (Some(x), Some(y)) => {
                    key -> x.combine(y, condType)
                }
                case _ => {
                    throw new NoSuchElementException(s"Key $key not found in some mapping")
                }
            }
        }.toMap

        new DeltaCollection(newMapping)
    }; 

    def concat(other: DeltaCollection) : DeltaCollection = {
        // Combines two DeltaCollection. the `this` Collection happend before the `other` Collection.

        var newDependencies = Map[String, DeltaMapping]()
        other.collection.map({case (key, values) => {
            // key, depends on the values. We need to combine the current dependency with the dependency from the previous (this) collection.
            
            // mapping that contains the dependencies of key
            var mapping = Map[String, HyperTypeCollection]()
            values.mapping.map({case (depKey, depValue) => {
                // the value of key, depends on the value of depKey with relation depValue.
                // We now need to find the dependencies of depKey in the previous collection (this.collection)

                if (this.collection.contains(depKey)) {
                    // The depKey exists in the previous collection. Hence we need to combine the dependencies.
                    val previousMapping = this.collection(depKey)

                    previousMapping.mapping.map({case (prevKey, prevValue) => {
                        val infFlowType = prevValue.joinInfFlow(depValue)
                        val valueType = prevValue.concatValue(depValue)
                        // TODO mono type etc.
                        if (mapping.contains(prevKey)) {
                            // If the key is already in the mapping, we need to combine the dependencies.
                            val existingDependency = mapping(prevKey)
                            val collectionBefore = new HyperTypeCollection(infFlowType, valueType)
                            val combinedInfFlow = existingDependency.joinInfFlow(collectionBefore)
                            val combinedValue = existingDependency.joinValue(collectionBefore)
                            mapping += (prevKey -> HyperTypeCollection(informationFlow = combinedInfFlow, value = combinedValue))
                        } else {
                            // If the key is not in the mapping, we can just add it.
                            mapping += (prevKey -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
                        }
                    }})
                } else {
                    // If the key is not in the previous collection, we can just add the dependency.
                    mapping += (depKey -> depValue)
                } 
                // newDependencies += (key -> mapping)
            }
            newDependencies += (key -> DeltaMapping(mapping))
            
        })
        

        // Now we have a new collection with the combined dependencies.
    }})
    val leftOverKeys = this.collection.keySet.diff(newDependencies.keySet)

    leftOverKeys.foreach(key => {
        // If the key is not in the new dependencies, we can just add it.
        newDependencies += (key -> this.collection(key))
    })
    DeltaCollection(newDependencies)

    }
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

    def isEmpty: Boolean = {
        collection.isEmpty 
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

    def combine(other: DeltaMapping, condType: HyperTypeCollection) : DeltaMapping = {
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
                        case _ => HyperTypeCollection(informationFlow=x.joinInfFlow(y)).joinInfFlow(condType)
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
    def isEmpty: Boolean = {
        mapping.isEmpty
    }

}


object DeltaMapping {

    def combineConstantCommutativeOp(firstDelta : HyperTypeCollection, firstType : HyperTypeCollection, secondType: HyperTypeCollection, op: String) : Option[HyperTypeCollection] = {
        // Combines one part that depends on variable and a constant part. This function is only used for commutative operations like + and *. 
        // If None is returned, it means that the result is no longer dependent on the variable (e.g. 0 * x)
        op match {
            case "+" => {
                val infFlowType = firstDelta.joinInfFlow(secondType);
                (firstDelta.value, secondType.value) match {
                    case (None, _) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    }
                    case (_, None) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    }
                    case (Some(Pos()) , Some(Pos())) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                    }
                    case (Some(Neg()), Some(Neg())) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                    }
                    case (_, Some(Zero())) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = firstDelta.value))
                    }
                    case (Some(Zero()), _) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = secondType.value))
                    }
                    case (_, _) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType))
                    }
                } 
            }
            case "*" => {
                val infFlowType = firstDelta.joinInfFlow(secondType);
                (firstDelta.value, secondType.value) match {
                    case (None, _) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    }
                    case (_, None) => {
                        Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    }
                    case (_ , Some(Zero())) => {
                        None
                    }
                    case (Some(Pos()), Some(value)) => {
                        // delta = x' - x > 0. What is delta' = x' * second - x?
                        (value, secondType.absValue) match {
                            case (_, None) => Some(HyperTypeCollection(informationFlow = infFlowType))
                            case (Pos(), Some(GreaterOne())) => {
                                // Multiplication with a value greater than 1. Hence the difference will increase.
                                Some(HyperTypeCollection(informationFlow = High(), value = Some(Pos())))
                            }
                            case (Pos(), Some(One())) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                            case (_, _) => {
                                Some(HyperTypeCollection(informationFlow = High()))
                            }
                        }
                    }
                    case (Some(Neg()), Some(value)) => {
                        // delta = x' - x < 0. Hence we know x' < x
                        (value, secondType.absValue) match {
                            case (_, None) => Some(HyperTypeCollection(informationFlow = High()))
                            case (Pos(), Some(GreaterOne())) => Some(HyperTypeCollection(informationFlow = High(), value = Some(Neg())))
                            case (Pos(), Some(One())) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                            case (_, _) => Some(HyperTypeCollection(informationFlow = High()))
                        }
                    }
                    case (Some(Zero()), Some(value)) => {
                        // x' - x = 0. What is x'* second - x?
                        // x' = x. For multiplication we have:
                            // increase x'' => pos
                            // decrease x'' => neg
                            // unchanged x'' => zero

                        // unchanged x'' multiplication by +1
                        if (value == Pos() && secondType.absValue.isDefined && secondType.absValue.get == One()) {
                            Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Zero())))
                        } else {
                            firstType.value match {
                                case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                                case Some(Zero()) => None
                                case Some(Pos()) => {
                                    // x' > 0. 
                                    if (value == Pos() && secondType.absValue.isDefined && secondType.absValue.get == GreaterOne()) {
                                        // Multiplication with a value greater than 1. Hence the difference will increase.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Pos())))
                                    } else if (value == Pos() && secondType.absValue.isDefined && secondType.absValue.get == LessOne()) {
                                        // Multiplication with a value less than 1. Hence the difference will decrease.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Neg())))
                                    } else if (value == Neg()) {
                                        // Multiplication with a negative value. Hence the difference will decrease.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Neg())))
                                    } else {
                                        Some(HyperTypeCollection(informationFlow = High(), value = None))
                                    }
                                }
                                case Some(Neg()) => {
                                    // x' < 0
                                    if (value == Pos() && secondType.absValue.isDefined && secondType.absValue.get == GreaterOne()) {
                                        // Multiplication with a value greater than 1. Hence the difference will decrease.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Neg())))
                                    } else if (value == Pos() && secondType.absValue.isDefined && secondType.absValue.get == LessOne()) {
                                        // Multiplication with a value less than 1. Hence the difference will increase.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Pos())))
                                    } else if (value == Neg()) {
                                        // Multiplication with a negative value. Hence the difference will increase.
                                        Some(HyperTypeCollection(informationFlow = High(), value = Some(Pos())))
                                    } else {
                                        Some(HyperTypeCollection(informationFlow = High(), value = None))
                                    }
                                }
                            }
                        }
                    }
                    case (_, _) => {
                        // Base Case
                        Some(HyperTypeCollection(informationFlow = infFlowType))
                    }
                }
            }
            case _ => Some(HyperTypeCollection(informationFlow = High()))
        }
    }

    def combineConstantOpLeft(leftDelta : HyperTypeCollection, leftType: HyperTypeCollection, rightType: HyperTypeCollection, op: String): Option[HyperTypeCollection] = {
        // Combines one part that depends on variable and a constant part. This function is only used for non-commutative operations like - and / where the non-constant part is on the left side.
        op match {
            case "-" => {
                val infFlowType = leftDelta.joinInfFlow(rightType);
                (leftDelta.value) match {
                    case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    case Some(Zero()) => {
                        // x' - x = 0 and x'' = x' - rightType. What is x'' - x
                        rightType.value match {
                            case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Pos()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                            case Some(Zero()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Zero())))
                            case _ => throw new IllegalArgumentException(s"Unsupported value for rightType: ${rightType.value}")
                        }
                    }
                    case Some(Neg()) => {
                        // x' - x < 0 and x'' = x' - rightType. What is x'' - x?
                        rightType.value match {
                            case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Pos()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Zero()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                            case _ => throw new IllegalArgumentException(s"Unsupported value for rightType: ${rightType.value}")
                        }
                    }
                    case Some(Pos()) => {
                        // x' - x > 0 and x'' = x' - rightType. What is x'' - x?
                        rightType.value match {
                            case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Pos()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                            case Some(Zero()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                            case _ => throw new IllegalArgumentException(s"Unsupported value for rightType: ${rightType.value}")
                        }
                    }
                    case _ => throw new IllegalArgumentException(s"Unsupported value for leftDelta: ${leftDelta.value}")  
                }
            }
            case "/" => {
                val infFlowType = leftDelta.joinInfFlow(rightType);
                (leftDelta.value) match {
                    case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    case Some(Zero()) => {
                        // x' - x = 0 and x'' = x' / rightType. What is x'' - x?
                        rightType.value match {
                            case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case Some(Pos()) => {
                                (rightType.absValue) match {
                                    case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                                    // division by one
                                    case Some(One()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Zero())))
                                    // division by greater than one
                                    case Some(GreaterOne()) => {
                                        leftType.value match {
                                            case Some(Zero()) => None
                                            case Some(Pos()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                                            case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                                        }
                                    
                                    }
                                    // division by less than one but positive
                                    case Some(LessOne()) => {
                                        leftType.value match {
                                            case Some(Zero()) => None
                                            case Some(Pos()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                                            case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                                        }
                                    }
                                }
                            }
                            case Some(Neg()) => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                        }
                    }
                    case Some(Pos()) => {
                        // x' - x > 0 and x'' = x' / rightType. What is x'' - x
                        // x' > x. We need to find divisions which increase the value of x'' or leave it unchanged.
                        if (
                            // Case one: left type is positive
                            leftType.value.isDefined && leftType.value.get == Pos() && (
                                // Divition by one or smaller one, results in a greater value
                                rightType.value.isDefined && rightType.value.get == Pos() && (
                                    rightType.absValue.isDefined && rightType.absValue.get == One() ||
                                    rightType.absValue.isDefined && rightType.absValue.get == LessOne()
                                )
                            ) ||
                            leftType.value.isDefined && leftType.value.get == Neg() && (
                                // Division by one or greater than one will result in a greater value after division.
                                rightType.value.isDefined && rightType.value.get == Pos() && (
                                    rightType.absValue.isDefined && rightType.absValue.get == One() ||
                                    rightType.absValue.isDefined && rightType.absValue.get == GreaterOne()
                                ) || 
                                // Division by a negative value, results in a greater value
                                rightType.value.isDefined && rightType.value.get == Neg()

                            )
                        ) {
                            Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                        } else {
                            Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                        }
                    }
                    case Some(Neg()) => {
                        // x' - x < 0 and x'' = x' / rightType. What is x'' - x?
                        // x' < x. We need to find divisions which decrease the value of x'' or leave it unchanged.
                        if (
                            // Case one: left type is positive
                            leftType.value.isDefined && leftType.value.get == Pos() && (
                                // Division by one or greater than one will result in a smaller value after division.
                                rightType.value.isDefined && rightType.value.get == Pos() && (
                                    rightType.absValue.isDefined && rightType.absValue.get == One() ||
                                    rightType.absValue.isDefined && rightType.absValue.get == GreaterOne()
                                ) || 
                                // Division by a negative value, results in a smaller value
                                rightType.value.isDefined && rightType.value.get == Neg()
                            ) ||
                            leftType.value.isDefined && leftType.value.get == Neg() && (
                                // Division by one or smaller than one, results in a smaller value
                                rightType.value.isDefined && rightType.value.get == Pos() && (
                                    rightType.absValue.isDefined && rightType.absValue.get == One() ||
                                    rightType.absValue.isDefined && rightType.absValue.get == LessOne()
                                )
                            )
                        ) {
                            Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                        } else {
                            Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                        }
                    }
                }
            }
            case _ => {
                throw new IllegalArgumentException(s"Unsupported operator: $op")
            }
        }
    }

    def combineConstantOpRight(leftType: HyperTypeCollection, leftDelta: HyperTypeCollection, rightType: HyperTypeCollection, op : String) : Option[HyperTypeCollection] = {
        op match {
            case "-" => {
                // x'' = leftType - x'
                // What is x'' - x?
                val infFlowType = leftDelta.joinInfFlow(rightType);
                (leftDelta.value) match {
                    case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    case Some(Zero()) => {
                        // x' - x = 0
                        (leftType.value, rightType.value) match {
                            case (None, _) => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case (_, None) => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                            case (Some(Pos()), Some(Neg())) | (Some(Pos()), Some(Zero())) | (Some(Zero()), Some(Neg())) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Pos())))
                            case (Some(Zero()), Some(Pos())) | (Some(Neg()), Some(Pos())) => Some(HyperTypeCollection(informationFlow = infFlowType, value = Some(Neg())))
                            case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                        }
                    }
                    // The other cases are very complicated and currently not supported.
                    case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                }
            }
            case "/" => {
                // x'' = leftType / x'
                // What is x'' - x?
                val infFlowType = leftDelta.joinInfFlow(rightType);
                (leftDelta.value) match {
                    case None => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                    // The other cases are very complicated and currently not supported.
                    case _ => Some(HyperTypeCollection(informationFlow = infFlowType, value = None))
                }
            }
            case _ => {
                throw new IllegalArgumentException(s"Unsupported operator: $op")
            }
        }
    }


    def combineOp(typeLeft : HyperTypeCollection, deltaLeft: DeltaMapping, typeRight: HyperTypeCollection, deltaRight  : DeltaMapping, op: String): DeltaMapping = {
        // Combines two DeltaMapping with a given operator. Returns the delta mapping for the result.
        val keysLeft = deltaLeft.mapping.keySet
        val keysRight = deltaRight.mapping.keySet
        val keysBoth = keysLeft.intersect(keysRight)

        val keysLeftOnly = keysLeft.diff(keysBoth)
        val keysRightOnly = keysRight.diff(keysBoth)
        var newMapping = Map[String, HyperTypeCollection]()
        keysLeftOnly.foreach(key => {
            val deltaType = deltaLeft.mapping(key);
            if (op == "+" || op == "*") {
                combineConstantCommutativeOp(deltaType, typeLeft, typeRight, op) match {
                    case Some(result) => newMapping += (key -> result)
                    case None => {}
                }
            } else if (op == "<" || op == "<=" || op == ">" || op == ">=" || op == "==" || op == "!=" || op == "&&" || op == "||") {
                newMapping += (key -> HyperTypeCollection(informationFlow = High(), value = None))
            } else if (op == "-" || op == "/") {
                combineConstantOpLeft(deltaType, typeLeft, typeRight, op) match {
                    case Some(result) => newMapping += (key -> result)
                    case None => {}
                }
            } else {
                throw new IllegalArgumentException(s"Unsupported operator: $op")
            }
        });

        keysRightOnly.foreach(key => {
            val deltaType = deltaRight.mapping(key);
            if (op == "+" || op == "*") {
                combineConstantCommutativeOp(deltaType, typeLeft, typeRight, op) match {
                    case Some(result) => newMapping += (key -> result)
                    case None => {}
                }
            } else if (op == "<" || op == "<=" || op == ">" || op == ">=" || op == "==" || op == "!=" || op == "&&" || op == "||") {
                newMapping += (key -> HyperTypeCollection(informationFlow = High(), value = None))
            } else if (op == "-" || op == "/") {
                combineConstantOpRight(typeLeft, deltaType, typeRight, op) match {
                    case Some(result) => newMapping += (key -> result)
                    case None => {}
                }
            } else {
                throw new IllegalArgumentException(s"Unsupported operator: $op")
            }
        });

        keysBoth.foreach(key => {
            // This is for stuff like this: (x - 1 + y) {+,-,*,/} (x + 1)
            // This becomes very complicated. Currently we do not support this.
            newMapping += (key -> HyperTypeCollection(informationFlow = High(), value = None))
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