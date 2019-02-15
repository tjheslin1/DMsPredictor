package io.github.tjheslin1.dmspredictor.classes

import cats.data.NonEmptyList
import cats.data.NonEmptyList.one
import cats.syntax.option._
import cats.syntax.traverse._
import cats.instances._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting._

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

    val triggerMet: Boolean   = true
    def conditionMet: Boolean = player.level >= levelRequirement

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          nextAbilityToUseInConjunction(combatant,
                                        order,
                                        NonEmptyList.of(ability.BonusAction, SingleAttack))
            .fold {
              val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)
              (updatedAttacker, updatedTarget.some)
            } { nextAbility =>
              val (updatedCombatant, updatedTargetOfAbility) =
                useAdditionalAbility(nextAbility, combatant, target)

              updatedTargetOfAbility.fold((updatedCombatant, none[Combatant])) { updatedTarget =>
                nextAbilityToUseInConjunction(updatedCombatant, order, one(SingleAttack)).fold {
                  val (updatedAttacker, updatedAttackedTarget) =
                    attackAndDamageTimes(1, updatedCombatant, updatedTarget)
                  (updatedAttacker, updatedAttackedTarget.some)
                } { nextAbility2 =>
                  useAdditionalAbility(nextAbility2, updatedCombatant, updatedTarget)
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

  def castSpell(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val player = combatant.creature.asInstanceOf[Player]

    val name             = "Cast Spell"
    val order            = currentOrder
    val levelRequirement = LevelThree
    val abilityAction    = WholeAction

    val triggerMet: Boolean = true
    def conditionMet: Boolean = player.spellSlots match {
      case None => false
      case Some(spellSlots) =>
        player.cantripKnown.isDefined || player.spellsKnown.isDefined &&
          player.level >= levelRequirement && available(spellSlots)
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

//      val spell = highestSpellSlotAvailable(player.spellSlots) match {
//        case None =>
//        case Some(spellLevel) => player.spellsKnown(spellLevel.spellLevel)
//      }

      (player.spellSlots, player.spellsKnown, target) match {
        case (_, _, None) => (combatant, none[Combatant])
        case (None, _, _) => (combatant, none[Combatant])
        case (Some(spellSlots), Some(target: Combatant)) =>
          val spell = (player.cantripKnown, highestSpellSlotAvailable(spellSlots)) match {
            case (cantrip, None) => cantrip
            case (_, Some(spellLevel)) =>
              player.spellsKnown.traverse(x => x) //(sk => sk(spellLevel.spellLevel))
          }

          val attackResult: AttackResult = spell.spellOffenseStyle match {
            case MeleeSpellAttack       => spellAttack(spell, target.creature)
            case RangedSpellAttack      => spellAttack(spell, target.creature)
            case SavingThrow(attribute) => spellSavingThrow(spell, attribute, target.creature)
          }

          val dmg = Math.max(
            0,
            attackResult match {
              case CriticalHit =>
                spell.damage(player.level) + spell.damage(player.level)
              case Hit          => spell.damage(player.level)
              case Miss         => 0
              case CriticalMiss => 0
            }
          )

          val adjustedDamage = spell.damageType match {
            case damageType if target.creature.resistances.contains(damageType) =>
              math.floor(dmg / 2).toInt
            case damageType if target.creature.immunities.contains(damageType) => 0
            case _                                                             => dmg
          }

          val damagedTarget =
            target.copy(creature = target.creature.updateHealth(Math.negateExact(adjustedDamage)))

          (combatant, damagedTarget.some)
      }
    }

    def update: Creature = {
      val spellSlotUsed         = highestSpellSlotAvailable(player.spellSlots)
      val updatedSpellSlotCount = spellSlotUsed.count - 1

      val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
        case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
      }

      (Player.spellSlotsLens composeLens spellSlotLens composeLens spellSlotCountLens)
        .set(updatedSpellSlotCount)(player)
    }

    private def spellAttack[_: RS](spell: Spell, target: Creature): AttackResult =
      D20.roll() match {
        case 20 => CriticalHit
        case 1  => CriticalMiss
        case roll =>
          if ((roll + spell.spellAttackBonus(player)) >= target.armourClass) Hit else Miss
      }

    private def spellSavingThrow[_: RS](spell: Spell,
                                        attribute: Attribute,
                                        target: Creature): AttackResult =
      if ((D20.roll() + attributeModifier(target, attribute)) >= spell.spellSaveDc(player))
        Miss
      else Hit
  }

}
