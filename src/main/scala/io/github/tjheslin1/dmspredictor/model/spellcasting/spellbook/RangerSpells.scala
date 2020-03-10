package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object RangerSpells extends LazyLogging {

  case object HuntersMarkBuffCondition extends PassiveCondition {
    val name               = "Hunter's Mark (bonus damage)"
    val saveDc             = 0 // caster maintains benefit until lost
    val turnsLeft          = 100 // lasts 1 hour
    val missesTurn         = false
    val isHandledOnDamage  = true
    val useHigherSpellSlot = false

    def decrementTurnsLeft(): Condition = this
  }

  def huntersMarkDamage[_: RS](): Int = 1 * D6

  case object HuntersMark extends SelfBuffSpell {
    val name              = "Hunter's Mark"
    val selfBuffCondition = HuntersMarkBuffCondition

    val school: SchoolOfMagic    = Divination
    val castingTime: CastingTime = BonusActionCast

    val spellLevel: SpellLevel = 1
    val requiresConcentration  = true
    val useHigherSpellSlot     = false
  }
}
