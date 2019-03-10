package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object WizardSpells {

  case object MagicMissile extends Spell {

    val name                               = "Chromatic Orb"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = DamageSpell(Force)
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def effect[_: RS](spellCaster: SpellCaster): Int = (3 * D4) + 3
  }
}
