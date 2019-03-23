package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Paralyzed}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells {

  case object SacredFlame extends SingleTargetSavingThrowSpell {
    val attribute: Attribute      = Dexterity
    val halfDamageOnSave: Boolean = false

    val damageType: DamageType         = Radiant
    val name                           = "Sacred Flame"
    val school: SchoolOfMagic          = Evocation
    val castingTime: CastingTime       = OneAction
    val spellLevel: SpellLevel         = 0
    val requiresConcentration: Boolean = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = spellCaster match {
      case p: Player if p.level == LevelFive => 2 * D8
      case _                                 => 1 * D8
    }
  }

  case object GuidingBolt extends SingleTargetAttackSpell {
    val damageType: DamageType             = Radiant
    val name                               = "Guiding Bolt"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val requiresConcentration: Boolean     = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (3 + spellLevel) * D6
  }

  case object CureWounds extends SingleTargetHealingSpell {
    val name                               = "Cure Wounds"
    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
    val spellLevel: SpellLevel             = 1
    val requiresConcentration: Boolean     = false

    def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      (spellLevel.value * D8) + attributeModifierForSchool(spellCaster)
  }

  case object HoldPerson extends SingleTargetConditionSpell {
    val name: String         = "Hold Person"
    val attribute: Attribute = Wisdom

    val school: SchoolOfMagic          = Enchantment
    val castingTime: CastingTime       = OneAction
    val spellLevel: SpellLevel         = 2
    val requiresConcentration: Boolean = true

    def conditionFrom(spellCaster: SpellCaster): Condition =
      Paralyzed(spellSaveDc(spellCaster), 10, attribute)
  }

  case object SpiritGuardians extends MultiTargetConditionSpell {
    val name = "Spirit Guardians"

    val attribute: Attribute           = Wisdom
    val school: SchoolOfMagic          = Conjuration
    val castingTime: CastingTime       = OneAction
    val spellLevel: SpellLevel         = 3
    val requiresConcentration: Boolean = true

    def conditionFrom(spellCaster: SpellCaster): Condition =
      SpiritGuardiansCondition(spellSaveDc(spellCaster), 100, Wisdom)
  }

  case class SpiritGuardiansCondition(saveDc: Int,
                                      turnsLeft: Int,
                                      attribute: Attribute,
                                      name: String = "Spirit Guardians (attack)")
      extends Condition {
    val missesTurn: Boolean     = false
    val handleOnDamage: Boolean = false

    def handle[_: RS](creature: Creature): Creature         = ???
    def handleOnDamage[_: RS](creature: Creature): Creature = creature
  }
}
