package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifier
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import monocle.Lens
import monocle.macros.GenLens

object EldritchKnightAbilities {

  import EldritchKnight._
  import EldritchKnightSpellSlots._

  def castSpell(combatant: Combatant): Ability = new Ability(combatant) {
    val eldritchKnight = combatant.creature.asInstanceOf[EldritchKnight]

    val levelRequirement: Level = LevelThree

    def triggerMet: Boolean   = true
    def conditionMet: Boolean = eldritchKnight.level >= levelRequirement && available(eldritchKnight.spellSlots)

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val spellSlot = highestSpellSlotAvailable(eldritchKnight.spellSlots)
      val spell     = eldritchKnight.spellsKnown(spellSlot.spellLevel)

      target match {
        case Some(target: Combatant) =>
          val attackResult: AttackResult = spell.spellOffenseStyle match {
            case MeleeSpellAttack       => spellAttack(spell, eldritchKnight)
            case RangedSpellAttack      => spellAttack(spell, eldritchKnight)
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

        case _ => case None => (combatant, None)
      }
    }

    def update: Creature = {
      val spellSlotUsed         = highestSpellSlotAvailable(eldritchKnight.spellSlots)
      val updatedSpellSlotCount = spellSlotUsed.count - 1

      val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
        case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
      }

      (_spellSlots composeLens spellSlotLens composeLens spellSlotCountLens).set(updatedSpellSlotCount)(eldritchKnight)
    }

    private def spellAttack(spell: Spell, target: Creature): AttackResult = D20.roll() match {
      case 20   => CriticalHit
      case 1    => CriticalMiss
      case roll => if ((roll + spell.spellAttackBonus(eldritchKnight)) >= target.armourClass) Hit else Miss
    }

    private def spellSavingThrow(spell: Spell, attribute: Attribute, target: Creature): AttackResult =
      if ((D20.roll() + attributeModifier(target, attribute)) >= spell.spellSaveDc(eldritchKnight)) Hit
      else Miss
  }

  val firstLevelSpellSlotLens: Lens[EldritchKnightSpellSlots, FirstLevelSpellSlot] =
    GenLens[EldritchKnightSpellSlots](_.firstLevel)
  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlot, Int] =
    GenLens[FirstLevelSpellSlot](_.count)
}
