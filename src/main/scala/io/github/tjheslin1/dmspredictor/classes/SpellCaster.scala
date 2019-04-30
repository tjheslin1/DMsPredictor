package io.github.tjheslin1.dmspredictor.classes

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import monocle.Lens

trait SpellCaster extends Creature {

  val spellsKnown: Map[(SpellLevel, SpellEffect), Spell]
  val spellSlots: SpellSlots
  val concentratingSpell: Option[Spell]

  val levelSpellcastingLearned: Level

  def isConcentrating: Boolean = concentratingSpell.isDefined

  def cantrip: Option[Spell] = spellsKnown.get((0, DamageSpell))
}

object SpellCaster {

  val concentratingLens: Lens[SpellCaster, Option[Spell]] =
    Lens[SpellCaster, Option[Spell]](_.concentratingSpell) { concentratingSpell =>
      {
        case c: Cleric => Cleric._concentratingSpell.set(concentratingSpell)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
    }

  val spellSlotsLens: Lens[SpellCaster, SpellSlots] = Lens[SpellCaster, SpellSlots](_.spellSlots) {
    spellSlots =>
      {
        case c: Cleric => Cleric._spellSlots.set(spellSlots)(c)

        case c: Wizard => Wizard._spellSlots.set(spellSlots)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
  }
}
