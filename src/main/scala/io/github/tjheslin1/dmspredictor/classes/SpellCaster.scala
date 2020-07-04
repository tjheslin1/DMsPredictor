package io.github.tjheslin1.dmspredictor.classes

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import monocle.Lens

trait SpellCaster extends Creature {

  val spellCastingModifier: Int
  val spellCastingAttribute: Attribute

  val spellsKnown: Map[(SpellLevel, SpellEffect), Spell]
  val spellSlots: SpellSlots
  val concentratingSpell: Option[Spell]

  val spellCastingLevel: Level
  val levelSpellcastingLearned: Level

  def isConcentrating: Boolean = concentratingSpell.isDefined

  def cantrip: Option[Spell] = spellsKnown.get((0, DamageSpell))
}

object SpellCaster {

  val concentratingLens: Lens[SpellCaster, Option[Spell]] =
    Lens[SpellCaster, Option[Spell]](_.concentratingSpell) { concentratingSpell =>
      {
        case c: Cleric => Cleric._concentratingSpell.set(concentratingSpell)(c)

        case c: Wizard => Wizard._concentratingSpell.set(concentratingSpell)(c)

        case c: Ranger => Ranger._concentratingSpell.set(concentratingSpell)(c)
        case c: Hunter => Hunter._concentratingSpell.set(concentratingSpell)(c)

        case c: Paladin => Paladin._concentratingSpell.set(concentratingSpell)(c)

        case c: Lich => Lich._concentratingSpell.set(concentratingSpell)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
    }

  val spellSlotsLens: Lens[SpellCaster, SpellSlots] = Lens[SpellCaster, SpellSlots](_.spellSlots) {
    spellSlots =>
      {
        case c: Cleric => Cleric._spellSlots.set(spellSlots)(c)

        case c: Wizard => Wizard._spellSlots.set(spellSlots)(c)

        case c: Ranger => Ranger._spellSlots.set(spellSlots)(c)
        case c: Hunter => Hunter._spellSlots.set(spellSlots)(c)

        case c: Paladin => Paladin._spellSlots.set(spellSlots)(c)

        case c: Lich => Lich._spellSlots.set(spellSlots)(c)

        case _ => throw new NotImplementedError("Missing a case in spellSlotsLens")
      }
  }
}
