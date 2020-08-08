package io.github.tjheslin1.dmspredictor.model

import cats.syntax.option._
import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots.highestSpellSlotAvailable
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf, Zombie}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire

package object spellcasting {

  type SpellLevel = Int Refined Interval.ClosedOpen[W.`0`.T, W.`10`.T]

  val LevelZero: SpellLevel = Refined.unsafeApply(0)

  sealed trait SchoolOfMagic extends Product with Serializable

  case object Evocation     extends SchoolOfMagic
  case object Abjuration    extends SchoolOfMagic
  case object Enchantment   extends SchoolOfMagic
  case object Conjuration   extends SchoolOfMagic
  case object Divination    extends SchoolOfMagic
  case object Necromancy    extends SchoolOfMagic
  case object Transmutation extends SchoolOfMagic

  sealed trait CastingTime extends Product with Serializable

  case object OneActionCast   extends CastingTime
  case object BonusActionCast extends CastingTime

  sealed trait SpellTargetStyle extends Product with Serializable

  case object MeleeSpellAttack                      extends SpellTargetStyle
  case object RangedSpellAttack                     extends SpellTargetStyle
  case class SpellSavingThrow(attribute: Attribute) extends SpellTargetStyle

  sealed trait SpellEffect extends Product with Serializable

  case object DamageSpellEffect        extends SpellEffect
  case object HealingSpellEffect       extends SpellEffect
  case object ConditionSpellEffect     extends SpellEffect
  case object BuffSpellEffect          extends SpellEffect
  case object InstantEffectSpellEffect extends SpellEffect

  def focusHigherHealthCreatureOrder(creature: Creature): Int =
    creature match {
      case _: Barbarian => 1
      case _: Berserker => 1
      case _: Fighter   => 2
      case _: Champion  => 2
      case _: Rogue     => 3
      case _: Ranger    => 4
      case _: Hunter    => 4
      case _: Paladin   => 5
      case _: Wizard    => 6
      case _: Cleric    => 7

      case _: Lich     => 8
      case _: Vampire  => 9
      case _: Werewolf => 10
      case _: Goblin   => 11
      case _: Zombie   => 12
    }

  def spellConditionMet(
      spellCaster: SpellCaster,
      effect: SpellEffect,
      singleTargetSpellsOnly: Boolean,
      multiTargetSpellsOnly: Boolean
  ): Boolean = {
    val optMaxSpellLevel = highestSpellSlotAvailable(spellCaster.spellSlots)
      .fold {
        spellCaster.cantrip.fold(none[SpellLevel])(_ => LevelZero.some)
      } {
        _.spellLevel.some
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
          case spell if singleTargetSpellsOnly && multiTargetSpellsOnly =>
            spell.isSingleTargetSpell || spell.isMultiTargetSpell
          case spell if singleTargetSpellsOnly => spell.isSingleTargetSpell
          case spell if multiTargetSpellsOnly  => spell.isMultiTargetSpell
          case _                               => true
        }
        .exists {
          case spell if spell.spellLevel.value <= maxSpellLevel.value =>
            spell.spellEffect match {
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

}
