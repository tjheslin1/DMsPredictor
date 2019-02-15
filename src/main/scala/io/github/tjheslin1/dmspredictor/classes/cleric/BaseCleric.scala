package io.github.tjheslin1.dmspredictor.classes.cleric

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellLevel}

abstract class BaseCleric extends Player with Product with Serializable {

  def turnReset(): Creature = this
}

object BaseCleric {

  val HitDice = D8

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (HitDice.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(HitDice) + mod(
      constitutionScore)))

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield) => Shield.armourClass(stats.dexterity)
      case _            => 0
    }

    baseArmourClass + shieldBonus
  }
}
