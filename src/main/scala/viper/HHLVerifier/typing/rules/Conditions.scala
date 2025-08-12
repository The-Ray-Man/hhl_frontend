package viper.HHLVerifier.typing.rules

import viper.HHLVerifier.ast.Num
import viper.HHLVerifier.typing.DeltaCollection
import viper.HHLVerifier.ast.Id
import viper.HHLVerifier.typing.dsl.HyperType
import viper.HHLVerifier.typing.HyperTypeCollection
import viper.HHLVerifier.ast.BoolLit

trait Condition {}

trait HyperTypeCondition extends Condition {
  def hyperApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, hyperTypeCollection: HyperTypeCollection): Boolean
}

trait DeltaCondition extends Condition {
  def deltaApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, deltaCollection: DeltaCollection): Boolean
}

trait SideCondition extends Condition {
  def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean
}

// case class ElementOf(val hyperType: HyperType) extends HyperTypeCondition {
//   override def hyperApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, hyperTypeCollection: HyperTypeCollection): Boolean = hyperTypeCollection.hypertypes.contains(hyperType)
// }

// case class NegateHyperCondition(condition: HyperTypeCondition) extends HyperTypeCondition {
//   override def hyperApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, hyperTypeCollection: HyperTypeCollection): Boolean = !condition.hyperApplies(context, checkContext, hyperTypeCollection)
// }

// case class DeltaContains(val varId: Int, val hyperType: HyperType) extends DeltaCondition {
//   override def deltaApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, deltaCollection: DeltaCollection): Boolean = {
//     checkContext.variables.length > varId && deltaCollection.mapping.contains(checkContext.variables(varId).name) && deltaCollection.mapping(checkContext.variables(varId).name).hypertypes.contains(hyperType)
//   }
// }
// case class VarNotInDelta(val varId: Int) extends DeltaCondition {
//   override def deltaApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, deltaCollection: DeltaCollection): Boolean = {
//     checkContext.variables.length >= varId || !deltaCollection.mapping.contains(checkContext.variables(varId).name)
//   }
// }

// case class NegateDeltaCondition(condition: DeltaCondition) extends DeltaCondition {
//   override def deltaApplies(context: ExpressionDerivationContext, checkContext: RuleCheckContext, deltaCollection: DeltaCollection): Boolean = !condition.deltaApplies(context, checkContext, deltaCollection)
// }

// case class VarInHyperMapping(val varId: Int) extends SideCondition {
//   override def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean = {
//     checkContext.variables.length > varId && context.mapping.mapping.contains(checkContext.variables(varId).name)
//   }
// }

// case class ExpressionIsVar(val varId: Int) extends SideCondition {
//   override def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean = {
//     context.expression match {
//       case id: Id => checkContext.variables.length > varId && id.name == checkContext.variables(varId).name
//       case _      => false
//     }
//   }
// }

case class ArithCondition(op: String, right: Int) extends SideCondition {
  override def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean = {
    context.expression match {
      case i: Num =>
        op match {
          case ">"  => i.value > right
          case "<"  => i.value < right
          case "==" => i.value == right
          case "!=" => i.value != right
          case ">=" => i.value >= right
          case "<=" => i.value <= right
          case _    => false
        }
      case _ => false
    }
  }
}

case class BoolCondition() extends SideCondition {
  override def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean = {
    context.expression match {
      case b: BoolLit => b.value
      case _          => false
    }
  }
}

case class NegateSideCondition(subCondition: SideCondition) extends SideCondition {
  override def applies(context: ExpressionDerivationContext, checkContext: RuleCheckContext): Boolean = !subCondition.applies(context, checkContext)
}
