package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, OnDamageCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object RangerSpells extends LazyLogging {

  case object HuntersMarkBuffCondition extends OnDamageCondition {
    val name           = "Hunter's Mark (bonus damage)"
    val saveDc         = 0 // caster maintains benefit until lost
    val turnsLeft      = 100 // lasts 1 hour
    val missesTurn     = false
    val handleOnDamage = true

    def decrementTurnsLeft(): Condition = this

    def handleOnDamage[_: RS](creature: Creature): Creature = ???
//      if (savingThrowPassed(saveDc, Constitution, creature)) {
//        val charmed           = creature.conditions.find(_.name == name).get
//        val updatedConditions = creature.conditions.except(charmed)
//
//        logger.debug(s"${creature.name} is no longer $name")
//
//        Creature.creatureConditionsLens.set(updatedConditions)(creature)
//      } else {
//        logger.debug(s"${creature.name} is still $name")
//        creature
//      }
  }

  def huntersMarkDamage[_: RS](): Int = 1 * D6

  case object HuntersMark extends SelfBuffSpell {
    val name              = "Hunter's Mark"
    val selfBuffCondition = HuntersMarkBuffCondition

    val school: SchoolOfMagic    = Divination
    val castingTime: CastingTime = BonusActionCast

    val spellLevel: SpellLevel = 1
    val requiresConcentration  = true

    def onLossOfConcentration(spellCaster: SpellCaster): SpellCaster = {
      val updatedConditions = spellCaster.conditions diff List(selfBuffCondition)

      Creature.creatureConditionsLens.set(updatedConditions)(spellCaster).asInstanceOf[SpellCaster]
    }
  }
}
