package viper.HHLVerifier.typing

import viper.HHLVerifier.ast.Stmt
import viper.HHLVerifier.management.PrettyPrinter

case class DeltaMapping(val collection: Map[String,DeltaMapping]) {

    override def equals(other: Any): Boolean = {
        other match {
            case that: DeltaMapping => {
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
        PrettyPrinter.formatDeltaMapping(this)
    }

    def isEmpty: Boolean = {
        collection.isEmpty 
    }
}

case class DeltaCollection(val mapping: Map[String, HyperTypeCollection]) {

    override def toString(): String = {
        PrettyPrinter.formatDeltaCollection(this)
    }

    override def equals(other: Any) : Boolean = {
        other match {
            case that: DeltaCollection => {
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