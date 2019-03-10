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
    val spellEffect: SpellEffect           = DamageSpell(Radiant)
    val spellTargetStyle: SpellTargetStyle = SpellSavingThrow(Dexterity)
    val spellLevel: SpellLevel             = 0
    val concentration: Boolean             = false

    def effect[_: RS](spellCaster: SpellCaster, targets: List[Combatant]): (SpellCaster, List[Combatant]) =
      spellCaster match {
      case p: Player if p.level == LevelFive => 2 * D8
      case _                                 => 1 * D8
    }
  }

  case object GuidingBolt extends Spell {
    val name                               = "Guiding Bolt"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = DamageSpell(Radiant)
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def effect[_: RS](spellCaster: SpellCaster, targets: List[Combatant]): (SpellCaster, List[Combatant]) = 4 * D6
  }

  case object CureWounds extends Spell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellEffect: SpellEffect           = HealingSpell
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def effect[_: RS](spellCaster: SpellCaster, targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
      val target = targets.head

      (1 * D8) + attributeModifierForSchool(spellCaster)
    }
  }

  case object HoldPerson extends Spell {
    val name: String = "Hold Person"
    val school: SchoolOfMagic = Enchantment
    val castingTime: CastingTime = OneAction
    val spellEffect: SpellEffect = ConditionSpell
    val spellTargetStyle: SpellTargetStyle = SpellSavingThrow(Wisdom)
    val spellLevel: SpellLevel = 2
    val concentration: Boolean = true

    def effect[_: RS](spellCaster: SpellCaster, targets: List[Combatant]): (SpellCaster, List[Combatant]) = ???
}
}
