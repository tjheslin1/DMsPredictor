package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting.MultiTargetBuffSpell.focusTanksCreatureOrder
import io.github.tjheslin1.dmspredictor.model.spellcasting._

object PaladinSpells {

  // TODO make an OnHitCondition
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
    val benefitsFromHigherSpellSlot = true
    val school                      = Enchantment

    val spellLevel: SpellLevel = 1
    val affectedTargets        = 2 + spellLevel

    val buffTargetsPriority: Ordering[Combatant] = (x: Combatant, y: Combatant) =>
      if (focusTanksCreatureOrder(x.creature) == focusTanksCreatureOrder(y.creature)) 0
      else if (focusTanksCrea
        tureOrder(x.creature) < focusTanksCreatureOrder(y.creature)) -1
      else 1
  }
}
