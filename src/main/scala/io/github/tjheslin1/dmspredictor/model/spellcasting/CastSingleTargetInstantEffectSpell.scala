package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CastSingleTargetInstantEffectSpell extends LazyLogging {

  def castSingleTargetInstantEffectSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name                         = "Cast Spell (Instant Effect)"
      val order                        = currentOrder
      val levelRequirement             = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean = spellConditionMet(
        spellCaster,
        InstantEffectSpellEffect,
        singleTargetSpellsOnly = true,
        multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val instantEffectCantrip: Option[Spell] = spellCaster.spellsKnown
          .find { spell =>
            spell.spellLevel.value == 0 && spell.spellEffect == InstantEffectSpellEffect
          }

        val (optSpell, foundSpellLevel) =
          highestSpellSlot match {
            case None =>
              (instantEffectCantrip, LevelZero)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, InstantEffectSpellEffect, spellSlot.spellLevel)(
                singleTargetSpellsOnly = true
              )
                .fold((instantEffectCantrip, LevelZero)) { case (foundSpell, spellLevel) =>
                  (foundSpell.some, spellLevel)
                }
          }

        val targets =
          spellCaster match {
            case _: Player =>
              monsters(others)
            case _ =>
              players(others)
          }

        val target = nextToFocus(combatant, targets, focus)

        (target, optSpell) match {
          case (_, None) =>
            (combatant, others)
          case (None, _) =>
            (combatant, others)
          case (Some(spellTarget), Some(spell)) =>
            val (spellAffectedCaster, List(updatedTarget)) = spell.effect(
              spellCaster,
              foundSpellLevel,
              List(spellTarget))

            val updatedSpellCaster =
              if (foundSpellLevel.value == 0) {
                spellAffectedCaster
              } else {
                val spellSlotUsed = spellSlotFromLevel(spellAffectedCaster, foundSpellLevel)

                decrementCastersSpellSlot(spellAffectedCaster, spellSlotUsed)
              }

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedTarget))
        }
      }

      def update: Creature = spellCaster
    }
}
