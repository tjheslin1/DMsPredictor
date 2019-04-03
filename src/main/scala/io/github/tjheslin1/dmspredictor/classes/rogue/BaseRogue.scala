package io.github.tjheslin1.dmspredictor.classes.rogue

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._

trait BaseRogue extends Player with Product with Serializable {

  def resetStartOfTurn(): Creature = this
  def scoresCritical(roll:  Int): Boolean = roll == 20
}

object BaseRogue {

  val HitDice = D8

  def sneakAttackDamage[_: RS] = Map(
    LevelOne -> 1 * D6,
    LevelTwo -> 1 * D6,
    LevelThree -> 2 * D6,
    LevelFour -> 2 * D6,
    LevelFive -> 3 * D6
  )

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def calculateArmourClass(stats: BaseStats, armour: Armour, offHand: Option[Equipment]): Int =
    armour.armourClass(stats.dexterity)
}
