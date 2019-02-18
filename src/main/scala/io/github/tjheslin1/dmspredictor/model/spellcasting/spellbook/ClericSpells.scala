package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object ClericSpells {

  case object SacredFlame extends Spell {

    val name                                 = "Sacred Flame"
    val school: SchoolOfMagic                = Evocation
    val castingTime: CastingTime             = OneAction
    val spellOffenseStyle: SpellOffenseStyle = SpellSavingThrow(Dexterity)
    val damageType: DamageType               = Radiant
    val spellLevel: SpellLevel               = 0

    def damage[_: RS](playerLevel: Level): Int = playerLevel match {
      case LevelFive => 2 * D8
      case _         => 1 * D8
    }
  }

  case object GuidingBolt extends Spell {
    val name                                 = "Guiding Bolt"
    val school: SchoolOfMagic                = Evocation
    val castingTime: CastingTime             = OneAction
    val spellOffenseStyle: SpellOffenseStyle = RangedSpellAttack
    val damageType: DamageType               = Radiant
    val spellLevel: SpellLevel               = 1

    def damage[_: RS](playerLevel: Level): Int = 4 * D6
  }
}
