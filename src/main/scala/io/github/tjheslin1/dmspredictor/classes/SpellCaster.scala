package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{EldritchKnight, SpellSlots}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellEffect, SpellLevel}
import monocle.Lens

trait SpellCaster extends Creature {

  val cantripKnown: Option[Spell]
  val spellsKnown: Map[(SpellLevel, SpellEffect), Spell]
  val spellSlots: SpellSlots
  val isConcentrating: Boolean

  val levelSpellcastingLearned: Level
}

object SpellCaster {

  val concentratingLens: Lens[SpellCaster, Boolean] =
    Lens[SpellCaster, Boolean](_.isConcentrating) { concentrating =>
      {
        case c: EldritchKnight => EldritchKnight._isConcentrating.set(concentrating)(c)
        case c: Cleric         => Cleric._isConcentrating.set(concentrating)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
    }

  val spellSlotsLens: Lens[SpellCaster, SpellSlots] = Lens[SpellCaster, SpellSlots](_.spellSlots) {
    spellSlots =>
      {
        case c: EldritchKnight => EldritchKnight._spellSlots.set(spellSlots)(c)
        case c: Cleric         => Cleric._spellSlots.set(spellSlots)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
  }
}
