package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.HyperType
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.BoolLit

trait Condition {}

trait HyperTypeCondition extends Condition {
    def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean
}

trait DeltaCondition extends Condition {
    def deltaApplies(deltaCollection: DeltaCollection): Boolean
}

trait SideCondition extends Condition {
    def applies(context: ExpressionDerivationContext): Boolean
}

case class ElementOf(val hyperType: HyperType) extends HyperTypeCondition {
    override def hyperApplies(hyperTypeCollection: HyperTypeCollection): Boolean = hyperTypeCollection.hypertypes.contains(hyperType)
}
case class DeltaContains(val variable : Id, val hyperType: HyperType) extends DeltaCondition {
    override def deltaApplies(deltaCollection: DeltaCollection): Boolean = false
}
case class VarNotInDelta(val variable: Id) extends DeltaCondition {
    override def deltaApplies(deltaCollection: DeltaCollection): Boolean = false
}

case class ConstPositiveInt() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value > 0
            case _ => false
        }
    }
}

case class ConstNegativeInt() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value < 0
            case _ => false
        }
    }
}

case class ConstZeroInt() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value == 0
            case _ => false
        }
    }
}

case class ConstAbsGtOne() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value.abs > 1
            case _ => false
        }
    }
}

case class ConstAbsLtOne() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value.abs < 1
            case _ => false
        }
    }
}

case class ConstAbsOne() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case i: Num => i.value.abs == 1
            case _ => false
        }
    }
}

case class ConstTrue() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case b: BoolLit => b.value
            case _ => false
        }
    }
}

case class ConstFalse() extends SideCondition {
    override def applies(context: ExpressionDerivationContext): Boolean = {
        context.expression match {
            case b: BoolLit => !b.value
            case _ => false
        }
    }
}