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
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Healing}
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
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean = {
        val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
          .fold(spellCaster.cantrip.fold(none[Int])(_ => 0.some))(_.spellLevel.value.some)

        val capableOfCasting = spellCaster match {
          case player: Player with SpellCaster =>
            player.level >= player.levelSpellcastingLearned
          case _ => true
        }

        optMaxSpellLevel.fold(false) { maxSpellLevel =>
          capableOfCasting &&
          spellCaster.spellsKnown.exists {
            case ((spellLvl, spellEffect: SpellEffect), _: SingleTargetAttackSpell)
                if spellLvl <= maxSpellLevel =>
              spellEffect match {
                case DamageSpell => true
                case _           => false
              }
            case ((spellLvl, spellEffect: SpellEffect), _: SingleTargetSavingThrowSpell)
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
            case None =>
              (spellCaster.cantrip, 0)
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, DamageSpell, spellSlot.spellLevel)()
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

      def update: Creature = updateSpellSlot(spellCaster, DamageSpell)
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

      def conditionMet: Boolean = spellConditionMet(spellCaster, HealingSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => none[(Spell, SpellLevel)]
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, HealingSpell, spellSlot.spellLevel)()
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
          (updatedCombatant, others.replace(updatedTarget))
        )
      }

      def update: Creature = updateSpellSlot(spellCaster, HealingSpell)
    }

  def castConcentrationSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Concentration)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = spellConditionMet(spellCaster, ConcentrationSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => none[(Spell, SpellLevel)]
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster, ConcentrationSpell, spellSlot.spellLevel)()
        }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        optSpell match {
          case None => (combatant, others)
          case Some((foundSpell, foundSpellLevel)) =>
            val (updatedSpellCaster, updatedTargets) =
              foundSpell.effect(spellCaster, foundSpellLevel, targets)

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedTargets))
        }
      }

      def update: Creature =
        updateSpellSlot(spellCaster, ConcentrationSpell, newlyConcentrating = true)
    }

  def castMultiTargetOffensiveSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (Multi Target Offensive)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean = {
        val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
          .fold(spellCaster.cantrip.fold(none[Int])(_ => 0.some))(_.spellLevel.value.some)

        val capableOfCasting = spellCaster match {
          case player: Player with SpellCaster =>
            player.level >= player.levelSpellcastingLearned
          case _ => true
        }

        optMaxSpellLevel.fold(false) { maxSpellLevel =>
          capableOfCasting &&
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
              spellOfLevelOrBelow(spellCaster, DamageSpell, spellSlot.spellLevel)(
                multiAttackOnly = true
              ).fold((none[Spell], 0)) {
                case (foundSpell, foundSpellLevel) =>
                  (foundSpell.some, foundSpellLevel)
              }
          }

        val targets = spellCaster match {
          case _: Player => monsters(others)
          case _         => players(others)
        }

        val updatedTargets = optSpell.fold(others) { spell =>
          targets.map { target =>
            val (_, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), List(target))

            updatedTarget
          }
        }

        (combatant, others.replace(updatedTargets))
      }

      def update: Creature = updateSpellSlot(spellCaster, DamageSpell, multiAttackSpellUsed = true)
    }

  def castSelfBuffSpell(currentOrder: Int, buffAction: AbilityAction = BonusAction)(
      combatant: Combatant
  ): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name             = "Cast Spell (self buff)"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = buffAction

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean = spellConditionMet(spellCaster, BuffSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell =
          highestSpellSlot match {
            case None => none[(Spell, SpellLevel)]
            case Some(spellSlot) =>
              spellOfLevelOrBelow(spellCaster, BuffSpell, spellSlot.spellLevel)()
          }

        optSpell.fold((combatant, others)) {
          case (foundSpell, foundSpellLevel) =>
            val (updatedSpellCaster, updatedOthers) =
              foundSpell.effect(spellCaster, foundSpellLevel, others)

            val updatedCombatant = Combatant.spellCasterOptional.set(updatedSpellCaster)(combatant)

            (updatedCombatant, others.replace(updatedOthers))
        }

      }

      def update: Creature = updateSpellSlot(spellCaster, BuffSpell)
    }

  def castSingleTargetInstantEffectSpell(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[SpellCaster]

      val name                         = "Cast Spell (Instant Effect)"
      val order                        = currentOrder
      val levelRequirement             = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = ???

      def conditionMet: Boolean = spellConditionMet(spellCaster, InstantEffectSpell)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) =
        ???

      def update: Creature = ???
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))

  def spellConditionMet(spellCaster: SpellCaster, effect: SpellEffect): Boolean = {
    val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
      .fold(spellCaster.cantrip.fold(none[Int])(_ => 0.some))(_.spellLevel.value.some)

    val capableOfCasting = spellCaster match {
      case player: Player with SpellCaster =>
        player.level >= player.levelSpellcastingLearned
      case _ => true
    }

    optMaxSpellLevel.fold(false) { maxSpellLevel =>
      capableOfCasting &&
      spellCaster.spellsKnown.exists {
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
      multiAttackSpellUsed: Boolean = false
  ): Creature =
    highestSpellSlotAvailable(spellCaster.spellSlots) match {
      case None => spellCaster
      case Some(spellSlotFound) =>
        spellOfLevelOrBelow(spellCaster, spellEffect, spellSlotFound.spellLevel)(
          checkCasterIsConcentrating = newlyConcentrating == false,
          multiAttackOnly = multiAttackSpellUsed
        ).fold {
          spellCaster
        } {
          case (_, foundSpellLevel) =>
            if (foundSpellLevel.value == 0) {
              spellCaster
            } else {
              val spellSlotUsed = spellSlotFromLevel(spellCaster, foundSpellLevel)

              val updatedSpellSlotCount = spellSlotUsed.count - 1

              val spellSlotLens = spellSlotUsed match {
                case FirstLevelSpellSlots(_)   => firstLevelLens
                case SecondLevelSpellSlots(_)  => secondLevelLens
                case ThirdLevelSpellSlots(_)   => thirdLevelLens
                case FourthLevelSpellSlots(_)  => fourthLevelLens
                case FifthLevelSpellSlots(_)   => fifthLevelLens
                case SixthLevelSpellSlots(_)   => sixthLevelLens
                case SeventhLevelSpellSlots(_) => seventhLevelLens
                case EighthLevelSpellSlots(_)  => eighthLevelLens
                case NinthLevelSpellSlots(_)   => ninthLevelLens
              }

              (SpellCaster.spellSlotsLens composeLens spellSlotLens)
                .set(updatedSpellSlotCount)(spellCaster.asInstanceOf[SpellCaster])
            }
        }
    }
}
