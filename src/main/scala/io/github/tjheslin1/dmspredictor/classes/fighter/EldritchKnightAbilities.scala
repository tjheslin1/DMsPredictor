package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifier
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.strategy.Ability.Action
import monocle.Lens
import monocle.macros.GenLens

object EldritchKnightAbilities {

  import EldritchKnightSpellSlots._

  def castSpell(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val eldritchKnight = combatant.creature.asInstanceOf[EldritchKnight]

    val name                    = "Cast Spell"
    val order = currentOrder
    val levelRequirement: Level = LevelThree
    val abilityAction = Action

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = eldritchKnight.level >= levelRequirement && available(eldritchKnight.spellSlots)

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val spellSlot = highestSpellSlotAvailable(eldritchKnight.spellSlots)
      val spell     = eldritchKnight.spellsKnown(spellSlot.spellLevel)

      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          val attackResult: AttackResult = spell.spellOffenseStyle match {
            case MeleeSpellAttack       => spellAttack(spell, target.creature)
            case RangedSpellAttack      => spellAttack(spell, target.creature)
            case SavingThrow(attribute) => spellSavingThrow(spell, attribute, target.creature)
          }

          val dmg = Math.max(
            0,
            attackResult match {
              case CriticalHit  => spell.damage + spell.damage
              case Hit          => spell.damage
              case Miss         => 0
              case CriticalMiss => 0
            }
          )

          val adjustedDamage = spell.damageType match {
            case damageType if target.creature.resistances.contains(damageType) => math.floor(dmg / 2).toInt
            case damageType if target.creature.immunities.contains(damageType)  => 0
            case _                                                              => dmg
          }

          val damagedTarget = target.copy(creature = target.creature.updateHealth(Math.negateExact(adjustedDamage)))

          (combatant, damagedTarget.some)
      }
    }

    def update: Creature = {
      val spellSlotUsed         = highestSpellSlotAvailable(eldritchKnight.spellSlots)
      val updatedSpellSlotCount = spellSlotUsed.count - 1

      val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
        case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
      }

      (EldritchKnight._spellSlots composeLens spellSlotLens composeLens spellSlotCountLens)
        .set(updatedSpellSlotCount)(eldritchKnight)
    }

    private def spellAttack[_: RS](spell: Spell, target: Creature): AttackResult = D20.roll() match {
      case 20   => CriticalHit
      case 1    => CriticalMiss
      case roll => if ((roll + spell.spellAttackBonus(eldritchKnight)) >= target.armourClass) Hit else Miss
    }

    private def spellSavingThrow[_: RS](spell: Spell, attribute: Attribute, target: Creature): AttackResult =
      if ((D20.roll() + attributeModifier(target, attribute)) >= spell.spellSaveDc(eldritchKnight)) Miss
      else Hit
  }

  val firstLevelSpellSlotLens: Lens[EldritchKnightSpellSlots, FirstLevelSpellSlot] =
    GenLens[EldritchKnightSpellSlots](_.firstLevel)

  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlot, Int] =
    GenLens[FirstLevelSpellSlot](_.count)
}
