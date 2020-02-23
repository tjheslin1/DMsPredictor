package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, OnDamageCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object RangerSpells extends LazyLogging {

  case object HuntersMarkCondition extends OnDamageCondition {
    val name: String            = "Hunter's Mark (bonus damage)"
    val saveDc: Int             = 0 // caster maintains benefit until lost
    val turnsLeft: Int          = 100 // lasts 1 hour
    val missesTurn: Boolean     = false
    val handleOnDamage: Boolean = true

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

  case object HuntersMark extends Spell {
    val name: String             = "Hunter's Mark"
    val school: SchoolOfMagic    = Divination
    val castingTime: CastingTime = BonusActionCast
    val spellEffect: SpellEffect = ConcentrationSpell

    val spellLevel: SpellLevel         = 1
    val requiresConcentration: Boolean = true

    def effect[_: RS](spellCaster: SpellCaster,
                      spellLevel: SpellLevel,
                      targets: List[Combatant]): (SpellCaster, List[Combatant]) = {

      val concentratingCaster = SpellCaster.concentratingLens.set(Some(HuntersMark))(spellCaster)

      val currentConditions = concentratingCaster.conditions
      val conditionAppliedCaster = Creature.creatureConditionsLens
        .set(currentConditions ++ List(HuntersMarkCondition))(spellCaster)
        .asInstanceOf[SpellCaster]

      (conditionAppliedCaster, targets)
    }
  }
}
