package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CastMultiTargetOffensiveSpell extends LazyLogging {

  def castMultiTargetOffensiveSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Multi Target Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          DamageSpellEffect,
          singleTargetSpellsOnly = false,
          multiTargetSpellsOnly = true)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val (optSpell, foundSpellLevel) =
          highestSpellSlot match {
            case None => (none[Spell], LevelZero)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, DamageSpellEffect, spellSlot.spellLevel)(
                multiTargetSpellsOnly = true
              ).fold((none[Spell], LevelZero)) {
                case (foundSpell, spellLevel) =>
                  (foundSpell.some, spellLevel)
              }
          }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        val (spellAffectedCaster, updatedTargets) = optSpell.fold((spellCaster, others)) { spell =>
          spell.effect(spellCaster, foundSpellLevel, targets)
        }

        val updatedSpellCaster = if (foundSpellLevel.value == 0) {
          spellAffectedCaster
        } else {
          val spellSlotUsed = spellSlotFromLevel(spellAffectedCaster, foundSpellLevel)

          decrementCastersSpellSlot(spellAffectedCaster, spellSlotUsed)
        }

        val updatedCombatant = Combatant.creatureLens.set(updatedSpellCaster)(combatant)

        (updatedCombatant, others.replace(updatedTargets))
      }

      def update: Creature = spellCaster
    }

}
