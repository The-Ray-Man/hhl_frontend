package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.Stmt



class DeltaMapping(val mapping: Map[String, HyperTypeCollection]) {

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
                case (Some(x), None) => (key, x)
                case (None, Some(y)) => (key, y)
                case _ => throw new NoSuchElementException(s"Key $key not found in either mapping")
            }
        }.toMap

        new DeltaMapping(newMapping)
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
            var infFlowType = deltaType.joinValue(typeRight);
            val valueType = deltaType.joinValue(typeRight);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                infFlowType = Some(Low())
                }
                case _ => {}
            }
            newMapping += (key -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
        });

        keysRightOnly.foreach(key => {
            val deltaType = deltaRight.mapping(key);
            var infFlowType = typeLeft.joinInfFlow(deltaType);
            val valueType = typeLeft.joinValue(deltaType);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                infFlowType = Some(Low())
                }
                case _ => {}
            }
            newMapping += (key -> HyperTypeCollection(informationFlow = infFlowType, value = valueType))
        });

        keysBoth.foreach(key => {
            val deltaTypeLeft = deltaLeft.mapping(key);
            val deltaTypeRight = deltaRight.mapping(key);
            var infFlowType = deltaTypeLeft.joinInfFlow(deltaTypeRight);
            val valueType = deltaTypeLeft.joinValue(deltaTypeRight, op);
            valueType match {
                case Some(True()) | Some(False()) | Some(Zero()) => {
                infFlowType = Some(Low())
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
        val keysAfterOnly = keysAfter.diff(keysBoth)

        var newMapping = Map[String, DeltaMapping]()

        keysBeforeOnly.foreach(key => {
            newMapping += (key -> before(key))
        });

        keysAfterOnly.foreach(key => {
            newMapping += (key -> after(key))
        });

        keysBoth.foreach(key => {
            newMapping += (key -> before(key).combine(after(key)))
        });

        newMapping
    }
}