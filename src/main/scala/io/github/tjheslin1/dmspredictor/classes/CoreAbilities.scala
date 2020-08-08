package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.{attackAndDamage, attackAndDamageTimes}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Healing}
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CoreAbilities extends LazyLogging {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CombatantAbility] = List(
    extraAttack(1)
  )

  def extraAttack(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val player = combatant.creature.asInstanceOf[Player]

      val name             = ExtraAttack
      val order            = currentOrder
      val levelRequirement = LevelFive
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = player.level >= levelRequirement

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        nextToFocus(combatant, monsters(others), focus) match {
          case None => (combatant, others)
          case Some(target) =>
            nextAbilityToUseInConjunction(combatant, others, order, NonEmptyList.of(SingleAttack))
              .fold {
                val (updatedAttacker, updatedTarget, updatedOthers) =
                  attackAndDamageTimes(2, combatant, target, others)

                (updatedAttacker, updatedOthers.replace(updatedTarget))
              } { nextAbility =>
                val (updatedCombatant, updatedOthers) =
                  useAdditionalAbility(nextAbility, combatant, others, focus)

                nextAbilityToUseInConjunction(
                  updatedCombatant,
                  updatedOthers,
                  order,
                  NonEmptyList.one(SingleAttack)
                ).fold {
                  nextToFocus(updatedCombatant, monsters(updatedOthers), focus).fold {
                    (updatedCombatant, updatedOthers)
                  } { focusTarget =>
                    val (updatedAttacker, updatedAttackedTarget, updatedOthers2) =
                      attackAndDamage(updatedCombatant, focusTarget, updatedOthers)

                    (updatedAttacker, updatedOthers2.replace(updatedAttackedTarget))
                  }
                } { nextAbility2 =>
                  useAdditionalAbility(nextAbility2, updatedCombatant, updatedOthers, focus)
                }
              }
        }
      }

      def update: Creature = player
    }

  def castConditionSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Condition)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          ConditionSpellEffect,
          singleTargetSpellsOnly = false,
          multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => none[(Spell, SpellLevel)]
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, ConditionSpellEffect, spellSlot.spellLevel)()
        }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        optSpell match {
          case None => (combatant, others)
          case Some((foundSpell, foundSpellLevel)) =>
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
          spell.effect(spellCaster, Refined.unsafeApply(foundSpellLevel), targets)
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

      def conditionMet: Boolean =
        spellConditionMet(
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

        optSpell.fold((combatant, others)) {
          case (foundSpell, foundSpellLevel) =>
            val (spellAffectedCaster, updatedOthers) =
              foundSpell.effect(spellCaster, foundSpellLevel, others)

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

  def castSingleTargetInstantEffectSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name                         = "Cast Spell (Instant Effect)"
      val order                        = currentOrder
      val levelRequirement             = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          InstantEffectSpellEffect,
          singleTargetSpellsOnly = true,
          multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val instantEffectCantrip: Option[Spell] =
          spellCaster.spellsKnown
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
                .fold((instantEffectCantrip, LevelZero)) {
                  case (foundSpell, spellLevel) =>
                    (foundSpell.some, spellLevel)
                }
          }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        val target = nextToFocus(combatant, targets, focus)

        (target, optSpell) match {
          case (_, None) => (combatant, others)
          case (None, _) => (combatant, others)
          case (Some(spellTarget), Some(spell)) =>
            val (spellAffectedCaster, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(foundSpellLevel), List(spellTarget))

            val updatedSpellCaster = if (foundSpellLevel.value == 0) {
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

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))
}
