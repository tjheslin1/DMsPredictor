package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifierForSchool
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells {

  case object SacredFlame extends Spell {

    val name                               = "Sacred Flame"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = DamageSpell
    val spellTargetStyle: SpellTargetStyle = SpellSavingThrow(Dexterity)
    val damageType: DamageType             = Radiant
    val spellLevel: SpellLevel             = 0

    def effect[_: RS](spellCaster: SpellCaster): Int = spellCaster match {
      case p: Player if p.level == LevelFive => 2 * D8
      case _                                 => 1 * D8
    }
  }

  case object GuidingBolt extends Spell {
    val name                               = "Guiding Bolt"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = DamageSpell
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val damageType: DamageType             = Radiant
    val spellLevel: SpellLevel             = 1

    def effect[_: RS](spellCaster: SpellCaster): Int = 4 * D6
  }

  case object CureWounds extends Spell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = HealingSpell
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val damageType: DamageType             = Radiant
    val spellLevel: SpellLevel             = 1

    def effect[_: RS](spellCaster: SpellCaster): Int =
      (1 * D8) + attributeModifierForSchool(spellCaster)
  }
}
