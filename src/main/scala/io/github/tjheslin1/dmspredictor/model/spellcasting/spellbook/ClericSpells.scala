package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Paralyzed}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.attributeModifierForSchool
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells {

  case object SacredFlame extends SingleTargetSavingThrowSpell {
    val attribute: Attribute = Dexterity
    val halfDamageOnSave: Boolean = false

    val damageType: DamageType = Radiant
    val name                               = "Sacred Flame"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellLevel: SpellLevel             = 0
    val concentration: Boolean             = false

    def damage[_: RS](spellCaster: SpellCaster): Int = spellCaster match {
      case p: Player if p.level == LevelFive => 2 * D8
      case _                                 => 1 * D8
    }
  }

  case object GuidingBolt extends SingleTargetAttackSpell {
    val damageType: DamageType = Radiant
    val name                               = "Guiding Bolt"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def damage[_: RS](spellCaster: SpellCaster): Int =  4 * D6
  }

  case object CureWounds extends SingleTargetHealingSpell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def healing[_: RS](spellCaster: SpellCaster): Int = (1 * D8) + attributeModifierForSchool(spellCaster)
  }

  case class HoldPerson(saveDc: Int) extends SingleTargetConditionSpell {
    val name: String = "Hold Person"

    val attribute: Attribute = Wisdom
    val condition: Condition = Paralyzed(saveDc, 10, attribute)

    val school: SchoolOfMagic = Enchantment
    val castingTime: CastingTime = OneAction
    val spellLevel: SpellLevel = 2
    val concentration: Boolean = true
}
}
