package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object PaladinSpells {

  case class BlessCondition(turnsLeft: Int = 10) extends PassiveCondition {
    val name       = "Bless (Condition)"
    val missesTurn = false

    def decrementTurnsLeft(): Condition = BlessCondition(turnsLeft - 1)
  }

  case object Bless extends MultiTargetBuffSpell {
    val name          = "Bless"
    val buffCondition = BlessCondition()

    val castingTime                 = OneActionCast
    val requiresConcentration       = true
    val benefitsFromHigherSpellSlot = false
    val school                      = Enchantment

    val spellLevel: SpellLevel = 1
    val affectedTargets        = 2 + spellLevel

    val buffTargetsPriority: Ordering[Combatant] = (x: Combatant, y: Combatant) =>
      if (focusHigherHealthCreatureOrder(x.creature) == focusHigherHealthCreatureOrder(y.creature))
        0
      else if (
        focusHigherHealthCreatureOrder(x.creature) < focusHigherHealthCreatureOrder(y.creature)
      ) -1
      else 1
  }

  def blessAttackBonus[_: RS](creature: Creature): Int = {
    val blessed = creature.conditions.exists {
      case BlessCondition(_) => true
      case _                 => false
    }

    if (blessed) 1 * D4
    else 0
  }
}
