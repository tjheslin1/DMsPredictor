package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
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

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      val enemies = monsters(others)

      nextToFocus(enemies, focus) match {
        case None => (combatant, none[Combatant])
        case Some(targetOfAbility) =>
          nextAbilityToUseInConjunction(combatant,
                                        enemies,
                                        order,
                                        NonEmptyList.of(ability.BonusAction, SingleAttack))
            .fold {
              val (updatedAttacker, updatedTarget) =
                attackAndDamageTimes(2, combatant, targetOfAbility)
              (updatedAttacker, updatedTarget.some)
            } { nextAbility =>
              val (updatedCombatant, updatedTargetOfAbility) =
                useAdditionalAbility(nextAbility, combatant, enemies, focus)

              updatedTargetOfAbility.fold((updatedCombatant, none[Combatant])) { updatedTarget =>
                val updatedEnemies = enemies.replace(updatedTarget)

                nextAbilityToUseInConjunction(updatedCombatant,
                                              updatedEnemies,
                                              order,
                                              one(SingleAttack))
                  .fold {
                    val (updatedAttacker, updatedAttackedTarget) =
                      attackAndDamageTimes(1, updatedCombatant, updatedTarget)
                    (updatedAttacker, updatedAttackedTarget.some)
                  } { nextAbility2 =>
                    useAdditionalAbility(nextAbility2, updatedCombatant, updatedEnemies, focus)
                  }
              }
            }
      }
    }

    def update: Creature =
      (Combatant.playerOptional composeLens Player.playerBonusActionUsedLens)
        .set(true)(combatant)
        .creature
  }

  def castOffensiveSpell(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val spellCaster = combatant.creature.asInstanceOf[Player with SpellCaster]

    val name             = "Cast Spell"
    val order            = currentOrder
    val levelRequirement = LevelOne
    val abilityAction    = WholeAction

    def triggerMet(others: List[Combatant]) = true
    def conditionMet: Boolean =
      spellCaster.level >= spellCaster.levelSpellcastingLearned &&
        (highestSpellSlotAvailable(spellCaster.spellSlots).isDefined || spellCaster.cantripKnown.isDefined)

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      val optSpell =
        (spellCaster.cantripKnown, highestSpellSlotAvailable(spellCaster.spellSlots)) match {
          case (cantrip, None)       => cantrip
          case (_, Some(spellLevel)) => spellCaster.spellsKnown(spellLevel.spellLevel).some
        }

      val enemies = monsters(others)
      val target = nextToFocus(enemies, focus)

      (target, optSpell) match {
        case (_, None) => (combatant, none[Combatant])
        case (None, _) => (combatant, none[Combatant])
        case (Some(spellTarget), Some(spell)) =>
          val attackResult = spell.spellOffenseStyle match {
            case MeleeSpellAttack  => spellAttack(spell, spellTarget.creature)
            case RangedSpellAttack => spellAttack(spell, spellTarget.creature)
            case SpellSavingThrow(attribute) =>
              if (spellSavingThrowPassed(spellCaster, spell, attribute, spellTarget.creature)) Miss
              else Hit
          }

          logger.debug(s"casting ${spell.name} - $attackResult")

          val dmg = Math.max(
            0,
            attackResult match {
              case CriticalHit =>
                spell.damage(spellCaster.level) + spell.damage(spellCaster.level)
              case Hit          => spell.damage(spellCaster.level)
              case Miss         => 0
              case CriticalMiss => 0
            }
          )

          val damagedTarget =
            spellTarget.copy(
              creature = spellTarget.creature.updateHealth(dmg, spell.damageType, attackResult))

          (combatant, damagedTarget.some)
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
        case 20 => CriticalHit
        case 1  => CriticalMiss
        case roll =>
          if ((roll + spell.spellAttackBonus(spellCaster)) >= target.armourClass) Hit else Miss
      }
  }
}
