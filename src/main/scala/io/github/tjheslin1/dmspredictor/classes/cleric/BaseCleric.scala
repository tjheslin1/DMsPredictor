package io.github.tjheslin1.dmspredictor.classes.cleric

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._

abstract class BaseCleric extends Player with SpellCaster {

  val levelSpellcastingLearned: Level = LevelOne

  def resetStartOfTurn[_: RS](): Creature = this
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
