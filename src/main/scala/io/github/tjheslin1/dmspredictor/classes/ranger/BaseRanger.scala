package io.github.tjheslin1.dmspredictor.classes.ranger

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._

trait BaseRanger extends Player with Product with Serializable {}

object BaseRanger {

  val HitDice = D10

  def calculateHealth(level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def weaponWithFightingStyle[_: RS](
      weapon: Weapon,
      fightingStyles: List[RangerFightingStyle]
  ): Weapon = ???
}
