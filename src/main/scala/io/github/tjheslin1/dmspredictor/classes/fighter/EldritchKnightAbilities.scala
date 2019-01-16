package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.FirstLevelSpellSlot
import io.github.tjheslin1.dmspredictor.strategy.Ability
import monocle.Lens
import monocle.macros.GenLens

object EldritchKnightAbilities {

  import EldritchKnight._
  import EldritchKnightSpellSlots._

  val firstLevelSpellSlotLens: Lens[EldritchKnightSpellSlots, FirstLevelSpellSlot] =
    GenLens[EldritchKnightSpellSlots](_.firstLevel)

  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlot, Int] =
    GenLens[FirstLevelSpellSlot](_.count)

  def castSpell(combatant: Combatant): Ability = new Ability(combatant) {
    val eldrichKnight = combatant.creature.asInstanceOf[EldritchKnight]

    val levelRequirement: Level = LevelThree

    def triggerMet: Boolean   = true
    def conditionMet: Boolean = eldrichKnight.level >= levelRequirement && available(eldrichKnight.spellSlots)

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = ???

    def update: Creature = {
      val updatedSpellSlotCount = eldrichKnight.spellSlots.firstLevel.count - 1

      (_spellSlots composeLens firstLevelSpellSlotLens composeLens firstLevelSpellSlotCountLens)
        .set(updatedSpellSlotCount)(eldrichKnight)
    }
  }
}
