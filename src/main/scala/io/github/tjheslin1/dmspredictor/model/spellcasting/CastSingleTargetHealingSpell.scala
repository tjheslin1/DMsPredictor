package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.Focus._
import io.github.tjheslin1.dmspredictor.strategy.Target._
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Healing}
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CastSingleTargetHealingSpell extends LazyLogging {

  val Priority = 1

  val CastSingleTargetHealingSpellName = "Cast Spell (Healing)"

  def castSingleTargetHealingSpell(currentOrder: Int, bonusHealing: Int = 0)(
      combatant: Combatant
  ): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = CastSingleTargetHealingSpellName
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean =
        healingSpellTriggerMet(others)

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          HealingSpellEffect,
          singleTargetSpellsOnly = true,
          multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => none[(Spell, SpellLevel)]
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, HealingSpellEffect, spellSlot.spellLevel)(
              singleTargetSpellsOnly = true
            )
        }

        val targets = spellCaster match {
          case _: Player => players(others)
          case _         => monsters(others)
        }

        val target = nextToFocus(combatant, targets, Healing)

        val (updatedCombatant, optHealedAlly) = (target, optSpell) match {
          case (_, None) => (combatant, None)
          case (None, _) => (combatant, None)
          case (Some(spellTarget), Some((foundSpell, foundSpellLevel))) =>
            val (spellAffectedCaster, List(updatedTarget)) =
              foundSpell.effect(spellCaster, foundSpellLevel, List(spellTarget))

            val updatedSpellCaster = if (foundSpellLevel.value == 0) {
              spellAffectedCaster
            } else {
              val spellSlotUsed = spellSlotFromLevel(spellAffectedCaster, foundSpellLevel)

              decrementCastersSpellSlot(spellAffectedCaster, spellSlotUsed)
            }

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            val updatedHealth =
              Math.min(
                updatedTarget.creature.maxHealth,
                updatedTarget.creature.health + bonusHealing
              )

            val updatedBonusHealingTarget =
              (Combatant.creatureLens composeLens Creature.creatureHealthLens)
                .set(updatedHealth)(updatedTarget)

            if (bonusHealing > 0) {
              logger.debug(
                s"${updatedBonusHealingTarget.creature.name} healed for $bonusHealing bonus healing"
              )
            }

            (updatedCombatant, updatedBonusHealingTarget.some)
        }

        optHealedAlly.fold((updatedCombatant, others))(updatedTarget =>
          (updatedCombatant, others.replace(updatedTarget)))
      }

      def update: Creature = spellCaster
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))
}
