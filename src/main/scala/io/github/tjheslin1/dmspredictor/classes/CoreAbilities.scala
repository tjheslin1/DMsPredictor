package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.Actions.{attackAndDamage, attackAndDamageTimes}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
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
            case ((_, spellEffect: SpellEffect), _) => spellEffect.isInstanceOf[DamageSpell]
          }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used $name")

        val optSpell =
          (spellCaster.cantripKnown, highestSpellSlotAvailable(spellCaster.spellSlots)) match {
            case (cantrip, None) => cantrip
            case (_, Some(spellSlot)) =>
              spellCaster.spellsKnown((spellSlot.spellLevel, DamageSpell)).some
          }

        val enemies = monsters(others)
        val target  = nextToFocus(enemies, focus)

        (target, optSpell) match {
          case (_, None) => (combatant, others)
          case (None, _) => (combatant, others)
          case (Some(spellTarget), Some(spell)) =>

            // TODO move all to Spell#effect

            val attackResult = spell.spellTargetStyle match {
              case MeleeSpellAttack  => spellAttack(spell, spellTarget.creature)
              case RangedSpellAttack => spellAttack(spell, spellTarget.creature)
              case SpellSavingThrow(attribute) =>
                if (spellSavingThrowPassed(spellCaster, spell, attribute, spellTarget.creature))
                  Miss
                else Hit
            }

            logger.debug(s"casting ${spell.name} - $attackResult")

            val dmg = Math.max(
              0,
              attackResult match {
                case CriticalHit =>
                  spell.effect(spellCaster) + spell.effect(spellCaster)
                case Hit          => spell.effect(spellCaster)
                case Miss         => 0
                case CriticalMiss => 0
              }
            )

            val damagedTarget =
              spellTarget.copy(
                creature = spellTarget.creature.updateHealth(dmg, spell.damageType, attackResult))

            (combatant, others.replace(damagedTarget))
        }
      }

      def update: Creature =
        highestSpellSlotAvailable(spellCaster.spellSlots) match {
          case None => spellCaster
          case Some(spellSlotUsed) =>
            val updatedSpellSlotCount = spellSlotUsed.count - 1

            val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
              case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
            }

            (SpellCaster.spellSlotsLens composeLens spellSlotLens composeLens spellSlotCountLens)
              .set(updatedSpellSlotCount)(spellCaster.asInstanceOf[SpellCaster])
        }

      private def spellAttack[_: RS](spell: Spell, target: Creature): AttackResult =
        D20.roll() match {
          case roll if spellCaster.scoresCritical(roll) => CriticalHit
          case 1  => CriticalMiss
          case roll =>
            if ((roll + spell.spellAttackBonus(spellCaster)) >= target.armourClass) Hit else Miss
        }
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

        val optSpell = healingSpellInHighestSlot(spellCaster)

        val allies = players(others)
        val target = nextToFocus(allies, focus)

        val optHealedAlly = (target, optSpell) match {
          case (_, None) => None
          case (None, _) => None
          case (Some(spellTarget), Some(spell)) =>

            // TODO CureWounds#effect should apply healing. this ability will add the bonus
            val healing = spell.effect(spellCaster) + bonusHealing

            val updatedHealth =
              Math.min(spellTarget.creature.maxHealth, spellTarget.creature.health + healing)

            logger.debug(s"${spellTarget.creature.name} was healed for $healing")

            (Combatant.creatureLens composeLens Creature.creatureHealthLens)
              .set(updatedHealth)(spellTarget)
              .some
        }

        optHealedAlly.fold((combatant, others))(updatedTarget =>
          (combatant, others.replace(updatedTarget)))
      }

      def update: Creature =
        highestSpellSlotAvailable(spellCaster.spellSlots) match {
          case None => spellCaster
          case Some(spellSlotUsed) =>
            val updatedSpellSlotCount = spellSlotUsed.count - 1

            val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
              case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
            }

            (SpellCaster.spellSlotsLens composeLens spellSlotLens composeLens spellSlotCountLens)
              .set(updatedSpellSlotCount)(spellCaster.asInstanceOf[SpellCaster])
        }
    }

  def healingSpellTriggerMet(others: List[Combatant]): Boolean =
    players(others).exists(player => player.creature.health <= (player.creature.maxHealth / 2))

  def healingSpellConditionMet(spellCaster: Player with SpellCaster): Boolean =
    spellCaster.level >= spellCaster.levelSpellcastingLearned &&
      highestSpellSlotAvailable(spellCaster.spellSlots).isDefined &&
      spellCaster.spellsKnown.exists {
        case ((_, spellEffect: SpellEffect), _) => spellEffect.isInstanceOf[HealingSpell]
      }

  def healingSpellInHighestSlot(spellCaster: SpellCaster): Option[Spell] =
    highestSpellSlotAvailable(spellCaster.spellSlots) match {
      case None => None
      case Some(spellSlot) =>
        spellCaster.spellsKnown((spellSlot.spellLevel, HealingSpell)).some
    }
}
