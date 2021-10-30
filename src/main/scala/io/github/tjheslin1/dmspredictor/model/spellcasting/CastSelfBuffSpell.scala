package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CastSelfBuffSpell extends LazyLogging {

  def castSelfBuffSpell(currentOrder: Int, buffAction: AbilityAction = BonusAction)(
      combatant: Combatant
  ): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Self Buff)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = buffAction

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean = spellConditionMet(
        spellCaster,
        BuffSpellEffect,
        singleTargetSpellsOnly = true,
        multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell =
          highestSpellSlot match {
            case None => none[(Spell, SpellLevel)]
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, BuffSpellEffect, spellSlot.spellLevel)(
                singleTargetSpellsOnly = true
              )
          }

        optSpell.fold((combatant, others)) { case (foundSpell, foundSpellLevel) =>
          val (spellAffectedCaster, updatedOthers) = foundSpell.effect(
            spellCaster,
            foundSpellLevel,
            others)

          val updatedSpellCaster =
            if (foundSpellLevel.value == 0) {
              spellAffectedCaster
            } else {
              val spellSlotUsed = spellSlotFromLevel(spellAffectedCaster, foundSpellLevel)

              decrementCastersSpellSlot(spellAffectedCaster, spellSlotUsed)
            }

          val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

          (updatedCombatant, others.replace(updatedOthers))
        }
      }

      def update: Creature = spellCaster
    }
}
