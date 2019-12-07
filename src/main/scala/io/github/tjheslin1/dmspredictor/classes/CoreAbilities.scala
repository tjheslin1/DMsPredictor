package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions.{attackAndDamage, attackAndDamageTimes}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
import io.github.tjheslin1.dmspredictor.strategy.{Focus, PlayerHealing}
import io.github.tjheslin1.dmspredictor.util.ListOps._

object CoreAbilities extends LazyLogging {

  val ExtraAttack = "Extra Attack"

  val standardCoreAbilities: List[CombatantAbility] = List(
    extraAttack(1)
  )

  def extraAttack(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
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
                one(SingleAttack)
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
      val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

      val name             = "Cast Spell (Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean = {
        val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
          .fold(spellCaster.cantrip.fold(none[Int])(_ => 0.some))(_.spellLevel.value.some)

        optMaxSpellLevel.fold(false) { maxSpellLevel =>
          spellCaster.level >= spellCaster.levelSpellcastingLearned &&
          spellCaster.spellsKnown.exists {
            case ((spellLvl, spellEffect: SpellEffect), _) if spellLvl <= maxSpellLevel =>
              spellEffect match {
                case DamageSpell => true
                case _           => false
              }
            case _ => false
          }
        }
      }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None =>
              (spellCaster.cantrip, 0)
            case Some(spellSlot) =>
              val optSpell =
                spellOfLevelOrBelow(spellCaster, DamageSpell, spellSlot.spellLevel)
              optSpell.fold((spellCaster.cantrip, 0)) { foundSpell =>
                (foundSpell.some, spellSlot.spellLevel)
              }
          }

        val target = nextToFocus(combatant, monsters(others), focus)

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

      def update: Creature = updateSpellSlot(spellCaster, DamageSpell)
    }

  val CastSingleTargetHealingSpellName = "Cast Spell (Healing)"
  def castSingleTargetHealingSpell(currentOrder: Int, bonusHealing: Int = 0)(
      combatant: Combatant
  ): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

      val name             = CastSingleTargetHealingSpellName
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = healingSpellTriggerMet(others)

      def conditionMet: Boolean = spellConditionMet(spellCaster, HealingSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => None
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, HealingSpell, spellSlot.spellLevel)
        }

        val target = nextToFocus(combatant, players(others), PlayerHealing)

        val (updatedCombatant, optHealedAlly) = (target, optSpell) match {
          case (_, None) => (combatant, None)
          case (None, _) => (combatant, None)
          case (Some(spellTarget), Some(spell)) =>
            val (updatedSpellCaster, List(updatedTarget)) =
              spell.effect(spellCaster, highestSpellSlot.get.spellLevel, List(spellTarget))

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
          (updatedCombatant, others.replace(updatedTarget))
        )
      }

      def update: Creature = updateSpellSlot(spellCaster, HealingSpell)
    }

  def castConcentrationSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

      val name             = "Cast Spell (Condition)"
      val order            = currentOrder
      val levelRequirement = LevelFive
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = spellConditionMet(spellCaster, ConcentrationSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => None
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, ConcentrationSpell, spellSlot.spellLevel)
        }

        optSpell match {
          case None => (combatant, others)
          case Some(spell) =>
            val (updatedSpellCaster, updatedTargets) =
              spell.effect(spellCaster, highestSpellSlot.get.spellLevel, monsters(others))

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedTargets))
        }
      }

      def update: Creature =
        updateSpellSlot(spellCaster, ConcentrationSpell, newlyConcentrating = true)
    }

  def castMultiTargetOffensiveSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

      val name             = "Cast Spell (Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean = {
        val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
          .fold(spellCaster.cantrip.fold(none[Int])(_ => 0.some))(_.spellLevel.value.some)

        optMaxSpellLevel.fold(false) { maxSpellLevel =>
          spellCaster.level >= spellCaster.levelSpellcastingLearned &&
          spellCaster.spellsKnown.exists {
            case ((spellLvl, spellEffect: SpellEffect), _: MultiTargetSavingThrowSpell)
                if spellLvl <= maxSpellLevel =>
              spellEffect match {
                case DamageSpell => true
                case _           => false
              }
            case _ => false
          }
        }
      }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val (optSpell, spellLevelToUse) =
          highestSpellSlot match {
            case None => (none[Spell], 0)
            case Some(spellSlot) =>
              val optSpell =
                spellOfLevelOrBelow(
                  spellCaster,
                  DamageSpell,
                  spellSlot.spellLevel,
                  multiAttackOnly = true
                )
              optSpell.fold((none[Spell], 0)) { foundSpell =>
                (foundSpell.some, spellSlot.spellLevel)
              }
          }

        val updatedTargets = optSpell.fold(others) { spell =>
          monsters(others).map { target =>
            val (_, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), List(target))

            updatedTarget
          }
        }

        (combatant, others.replace(updatedTargets))
      }

      def update: Creature = updateSpellSlot(spellCaster, DamageSpell)
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))

  def spellConditionMet(spellCaster: Player with SpellCaster, effect: SpellEffect): Boolean = {
    val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
      .fold(none[Int])(_.spellLevel.value.some)

    optMaxSpellLevel.fold(false) { maxSpellLevel =>
      spellCaster.level >= spellCaster.levelSpellcastingLearned &&
      spellCaster.spellsKnown.exists {
        case ((spellLvl, spellEffect), spell) if spellLvl <= maxSpellLevel =>
          spellEffect match {
            case `effect` if canCastConcentrationSpell(spellCaster, spell) => true
            case _                                                         => false
          }
        case _ => false
      }
    }
  }

  private def canCastConcentrationSpell(spellCaster: SpellCaster, spell: Spell): Boolean =
    if (spell.requiresConcentration)
      spellCaster.isConcentrating == false
    else
      true

  private def updateSpellSlot(
      spellCaster: SpellCaster,
      spellEffect: SpellEffect,
      newlyConcentrating: Boolean = false
  ): Creature =
    highestSpellSlotAvailable(spellCaster.spellSlots) match {
      case None => spellCaster
      case Some(spellSlotUsed) =>
        val optSpell =
          spellOfLevelOrBelow(
            spellCaster,
            spellEffect,
            spellSlotUsed.spellLevel,
            newlyConcentrating == false
          )

        optSpell.fold {
          spellCaster
        } { foundSpell =>
          if (foundSpell.spellLevel.value == 0) {
            spellCaster
          } else {
            val updatedSpellSlotCount = spellSlotUsed.count - 1

            val spellSlotLens = spellSlotUsed match {
              case FirstLevelSpellSlots(_)  => firstLevelLens
              case SecondLevelSpellSlots(_) => secondLevelLens
              case ThirdLevelSpellSlots(_)  => thirdLevelLens
            }

            (SpellCaster.spellSlotsLens composeLens spellSlotLens)
              .set(updatedSpellSlotCount)(spellCaster.asInstanceOf[SpellCaster])
              .asInstanceOf[Player with SpellCaster]
          }
        }
    }
}
