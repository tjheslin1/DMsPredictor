package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.Actions.{attackAndDamage, attackAndDamageTimes}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
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

      val enemies = monsters(others)

      nextToFocus(enemies, focus) match {
        case None => (combatant, others)
        case Some(target) =>
          nextAbilityToUseInConjunction(combatant, enemies, order, NonEmptyList.of(SingleAttack))
            .fold {
              val (updatedAttacker, updatedTarget) =
                attackAndDamageTimes(2, combatant, target)
              (updatedAttacker, others.replace(updatedTarget))
            } { nextAbility =>
              val (updatedCombatant, updatedTargets) =
                useAdditionalAbility(nextAbility, combatant, enemies, focus)

              val updatedEnemies = enemies.replace(updatedTargets)

              nextAbilityToUseInConjunction(updatedCombatant,
                                            updatedEnemies,
                                            order,
                                            one(SingleAttack))
                .fold {
                  nextToFocus(updatedEnemies, focus).fold(updatedCombatant, updatedEnemies) {
                    focusTarget =>
                      val (updatedAttacker, updatedAttackedTarget) =
                        attackAndDamage(updatedCombatant, focusTarget)
                      (updatedAttacker,
                       others.replace(updatedEnemies).replace(updatedAttackedTarget))
                  }
                } { nextAbility2 =>
                  val (updatedAttacker, updatedEnemies2) =
                    useAdditionalAbility(nextAbility2, updatedCombatant, updatedEnemies, focus)
                  (updatedAttacker, others.replace(updatedEnemies2))
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
      def conditionMet: Boolean =
        spellCaster.level >= spellCaster.levelSpellcastingLearned &&
          (highestSpellSlotAvailable(spellCaster.spellSlots).isDefined || spellCaster.cantripKnown.isDefined) &&
          spellCaster.spellsKnown.exists {
            case ((_, spellEffect: SpellEffect), _) =>
              spellEffect match {
                case DamageSpell => true
                case _           => false
              }
          }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val (optSpell, spellLevelToUse) =
          (spellCaster.cantripKnown, highestSpellSlot) match {
            case (cantrip, None) =>
              (cantrip, 0)
            case (cantrip, Some(spellSlot)) =>
              val optSpell =
                spellOfLevelOrBelow(spellCaster.spellsKnown, DamageSpell, spellSlot.spellLevel)
              optSpell.fold((cantrip, 0)) { foundSpell =>
                (foundSpell.some, spellSlot.spellLevel)
              }
          }

        val enemies = monsters(others)
        val target  = nextToFocus(enemies, focus)

        (target, optSpell) match {
          case (_, None) => (combatant, others)
          case (None, _) => (combatant, others)
          case (Some(spellTarget), Some(spell)) =>
            val (_, List(updatedTarget)) =
              spell.effect(spellCaster, Refined.unsafeApply(spellLevelToUse), List(spellTarget))

            (combatant, others.replace(updatedTarget))
        }
      }

      def update: Creature = updateSpellSlot(spellCaster, DamageSpell)

    }

  val CastSingleTargetHealingSpellName = "Cast Spell (Healing)"

  def castSingleTargetHealingSpell(currentOrder: Int, bonusHealing: Int = 0)(
      combatant: Combatant): Ability =
    new Ability(combatant) {
      val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

      val name             = CastSingleTargetHealingSpellName
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = healingSpellTriggerMet(others)

      def conditionMet: Boolean = healingSpellConditionMet(spellCaster)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val highestSpellSlot = highestSpellSlotAvailable(spellCaster.spellSlots)

        val optSpell = highestSpellSlot match {
          case None => None
          case Some(spellSlot) =>
            spellOfLevelOrBelow(spellCaster.spellsKnown, HealingSpell, spellSlot.spellLevel)
        }

        val allies = players(others)
        val target = nextToFocus(allies, focus)

        val optHealedAlly = (target, optSpell) match {
          case (_, None) => None
          case (None, _) => None
          case (Some(spellTarget), Some(spell)) =>
            val (_, List(updatedTarget: Combatant)) =
              spell.effect(spellCaster, highestSpellSlot.get.spellLevel, List(spellTarget))

            val updatedHealth =
              Math.min(updatedTarget.creature.maxHealth,
                       updatedTarget.creature.health + bonusHealing)

            val updatedBonusHealingTarget =
              (Combatant.creatureLens composeLens Creature.creatureHealthLens)
                .set(updatedHealth)(updatedTarget)

            if (bonusHealing > 0) {
              logger.debug(
                s"${updatedBonusHealingTarget.creature.name} healed for $bonusHealing bonus healing")
            }

            updatedBonusHealingTarget.some
        }

        optHealedAlly.fold((combatant, others))(updatedTarget =>
          (combatant, others.replace(updatedTarget)))
      }

      def update: Creature = updateSpellSlot(spellCaster, HealingSpell)
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))

  def healingSpellConditionMet(spellCaster: Player with SpellCaster): Boolean =
    spellCaster.level >= spellCaster.levelSpellcastingLearned &&
      highestSpellSlotAvailable(spellCaster.spellSlots).isDefined &&
      spellCaster.spellsKnown.exists {
        case ((_, spellEffect: SpellEffect), _) =>
          spellEffect match {
            case HealingSpell => true
            case _            => false
          }
      }

  private def updateSpellSlot(spellCaster: SpellCaster, spellEffect: SpellEffect): Creature =
    highestSpellSlotAvailable(spellCaster.spellSlots) match {
      case None => spellCaster
      case Some(spellSlotUsed) =>
        val optSpell =
          spellOfLevelOrBelow(spellCaster.spellsKnown, spellEffect, spellSlotUsed.spellLevel)

        optSpell.fold(spellCaster) { foundSpell =>
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
