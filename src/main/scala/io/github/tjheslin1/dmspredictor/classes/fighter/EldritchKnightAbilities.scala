package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import monocle.Lens
import monocle.macros.GenLens

object EldritchKnightAbilities {

  import EldritchKnight._
  import EldritchKnightSpellSlots._

  def castSpell(combatant: Combatant): Ability = new Ability(combatant) {
    val eldrichKnight = combatant.creature.asInstanceOf[EldritchKnight]

    val levelRequirement: Level = LevelThree

    def triggerMet: Boolean   = true
    def conditionMet: Boolean = eldrichKnight.level >= levelRequirement && available(eldrichKnight.spellSlots)

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val spellSlot = highestSpellSlotAvailable(eldrichKnight.spellSlots)
      val spell = eldrichKnight.spellsKnown(spellSlot.spellLevel)

      target match {
        case Some(target: Combatant) =>
          
        case _ => case None => (combatant, None)
      }

      ???
    }

    def update: Creature = {
      val spellSlotUsed         = highestSpellSlotAvailable(eldrichKnight.spellSlots)
      val updatedSpellSlotCount = spellSlotUsed.count - 1

      val (spellSlotLens, spellSlotCountLens) = spellSlotUsed match {
        case FirstLevelSpellSlot(_) => (firstLevelSpellSlotLens, firstLevelSpellSlotCountLens)
      }

      (_spellSlots composeLens spellSlotLens composeLens spellSlotCountLens).set(updatedSpellSlotCount)(eldrichKnight)
    }
  }

  val firstLevelSpellSlotLens: Lens[EldritchKnightSpellSlots, FirstLevelSpellSlot] =
    GenLens[EldritchKnightSpellSlots](_.firstLevel)
  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlot, Int] =
    GenLens[FirstLevelSpellSlot](_.count)
}
