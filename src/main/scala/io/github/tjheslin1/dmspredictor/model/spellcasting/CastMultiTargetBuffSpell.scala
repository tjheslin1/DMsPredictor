package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CastMultiTargetBuffSpell extends LazyLogging {

  def castMultiTargetBuffSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Multi Target Buff)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean =
        spellCaster match {
          case _: Player => players(others).nonEmpty
          case _         => monsters(others).nonEmpty
        }

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          BuffSpellEffect,
          singleTargetSpellsOnly = false,
          multiTargetSpellsOnly = true)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell =
          highestSpellSlot match {
            case None => none[(Spell, SpellLevel)]
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, BuffSpellEffect, spellSlot.spellLevel)(
                multiTargetSpellsOnly = true
              )
          }

        val targets = spellCaster match {
          case _: Player => players(others)
          case _         => monsters(others)
        }

        optSpell.fold((combatant, others)) {
          case (foundSpell, foundSpellLevel) =>
            val (spellAffectedCaster, updatedOthers) =
              foundSpell.effect(spellCaster, foundSpellLevel, targets)

            val updatedSpellCaster = if (foundSpellLevel.value == 0) {
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
