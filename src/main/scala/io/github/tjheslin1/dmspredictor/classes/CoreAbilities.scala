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

  def castSingleTargetOffensiveSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          DamageSpellEffect,
          singleTargetSpellsOnly = true,
          multiTargetSpellsOnly = false)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None =>
              (spellCaster.cantrip, 0)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, DamageSpellEffect, spellSlot.spellLevel)(
                singleTargetSpellsOnly = true)
                .fold((spellCaster.cantrip, 0)) {
                  case (foundSpell, foundSpellLevel) =>
                    (foundSpell.some, foundSpellLevel)
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
            val (updatedSpellCaster, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), List(spellTarget))

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedTarget))
        }
      }

      def update: Creature =
        updateSpellSlot(spellCaster, DamageSpellEffect, singleTargetSpellUsed = true)
    }

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
            spellOfLevelOrBelow(spellCaster, HealingSpellEffect, spellSlot.spellLevel)()
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
            val (updatedSpellCaster, List(updatedTarget)) =
              foundSpell.effect(spellCaster, foundSpellLevel, List(spellTarget))

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

      def update: Creature =
        updateSpellSlot(spellCaster, HealingSpellEffect, singleTargetSpellUsed = true)
    }

  def castConditionSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Multi Target Condition)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean =
        spellConditionMet(
          spellCaster,
          ConditionSpellEffect,
          singleTargetSpellsOnly = true,
          multiTargetSpellsOnly = true)

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

            val updatedSpellCaster = updateSpellSlot(
              spellAffectedCaster,
              ConditionSpellEffect,
              newlyConcentrating =
                spellAffectedCaster.isConcentrating && foundSpell.requiresConcentration
            ) // TODO specific tests
              .asInstanceOf[SpellCaster]

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

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None => (none[Spell], 0)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, DamageSpellEffect, spellSlot.spellLevel)(
                multiTargetSpellsOnly = true
              ).fold((none[Spell], 0)) {
                case (foundSpell, foundSpellLevel) =>
                  (foundSpell.some, foundSpellLevel)
              }
          }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        val (updatesSpellCaster, updatedTargets) = optSpell.fold((spellCaster, others)) { spell =>
          spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), targets)
        }

        val updatedCombatant = Combatant.creatureLens.set(updatesSpellCaster)(combatant)

        (updatedCombatant, others.replace(updatedTargets))
      }

      def update: Creature =
        updateSpellSlot(spellCaster, DamageSpellEffect, multiTargetSpellUsed = true)
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
              spellOfLevelOrBelow(spellCaster, BuffSpellEffect, spellSlot.spellLevel)()
          }

        optSpell.fold((combatant, others)) {
          case (foundSpell, foundSpellLevel) =>
            val (spellAffectedCaster, updatedOthers) =
              foundSpell.effect(spellCaster, foundSpellLevel, others)

            val updatedSpellCaster = updateSpellSlot(
              spellAffectedCaster,
              BuffSpellEffect,
              newlyConcentrating = foundSpell.requiresConcentration,
              singleTargetSpellUsed = true).asInstanceOf[SpellCaster]

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

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None => (none[Spell], 0)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, BuffSpellEffect, spellSlot.spellLevel)(
                multiTargetSpellsOnly = true
              ).fold((none[Spell], 0)) {
                case (foundSpell, foundSpellLevel) =>
                  (foundSpell.some, foundSpellLevel)
              }
          }

        val targets = spellCaster match {
          case _: Player => players(others)
          case _         => monsters(others)
        }

        val (updatesSpellCaster, updatedTargets) = optSpell.fold((spellCaster, others)) { spell =>
          spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), targets)
        }

        val updatedCombatant = Combatant.creatureLens.set(updatesSpellCaster)(combatant)

        (updatedCombatant, others.replace(updatedTargets))
      }

      def update: Creature =
        updateSpellSlot(
          spellCaster,
          BuffSpellEffect,
          multiTargetSpellUsed = true,
          newlyConcentrating = true
        ) // TODO should this always be true?
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
          spellCaster.spellsKnown.get((0, InstantEffectSpellEffect))

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None =>
              (instantEffectCantrip, 0)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, InstantEffectSpellEffect, spellSlot.spellLevel)()
                .fold((instantEffectCantrip, 0)) {
                  case (foundSpell, foundSpellLevel) =>
                    (foundSpell.some, foundSpellLevel)
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
            val (updatedSpellCaster, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), List(spellTarget))

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedTarget))
        }
      }

      def update: Creature = updateSpellSlot(spellCaster, InstantEffectSpellEffect)
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))

  def spellConditionMet(
      spellCaster: SpellCaster,
      effect: SpellEffect,
      singleTargetSpellsOnly: Boolean,
      multiTargetSpellsOnly: Boolean
  ): Boolean = {
    val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
      .fold {
        spellCaster.cantrip.fold(none[Int])(_ => 0.some)
      } {
        _.spellLevel.value.some
      }

    val capableOfCasting = spellCaster match {
      case player: Player with SpellCaster =>
        player.level >= player.levelSpellcastingLearned
      case _ => true
    }

    optMaxSpellLevel.fold(false) { maxSpellLevel =>
      capableOfCasting &&
      spellCaster.spellsKnown
        .filter {
          case (_, spell) if singleTargetSpellsOnly && multiTargetSpellsOnly =>
            singleTargetSpellOnly(spell) || multiTargetSpellOnly(spell)
          case (_, spell) if singleTargetSpellsOnly => singleTargetSpellOnly(spell)
          case (_, spell) if multiTargetSpellsOnly  => multiTargetSpellOnly(spell)
          case _                                    => true
        }
        .exists {
          case ((spellLvl, spellEffect), spell) if spellLvl <= maxSpellLevel =>
            spellEffect match {
              case `effect` if canCastSpell(spellCaster, spell) => true
              case _                                            => false
            }
          case _ => false
        }
    }
  }

  private def canCastSpell(spellCaster: SpellCaster, spell: Spell): Boolean =
    if (spell.requiresConcentration)
      spellCaster.isConcentrating == false
    else
      true

  private def updateSpellSlot(
      spellCaster: SpellCaster,
      spellEffect: SpellEffect,
      newlyConcentrating: Boolean = false,
      singleTargetSpellUsed: Boolean = false,
      multiTargetSpellUsed: Boolean = false
  ): Creature =
    highestSpellSlotAvailable(spellCaster.spellSlots) match {
      case None => spellCaster
      case Some(spellSlotFound) =>
        spellOfLevelOrBelow(spellCaster, spellEffect, spellSlotFound.spellLevel)(
          findNewlyConcentratingSpell = newlyConcentrating,
          singleTargetSpellsOnly = singleTargetSpellUsed,
          multiTargetSpellsOnly = multiTargetSpellUsed
        ).fold {
          spellCaster
        } {
          case (_, foundSpellLevel) =>
            if (foundSpellLevel.value == 0) {
              spellCaster
            } else {
              val spellSlotUsed = spellSlotFromLevel(spellCaster, foundSpellLevel)

              decrementCastersSpellSlot(spellCaster, spellSlotUsed)
            }
        }
    }
}
