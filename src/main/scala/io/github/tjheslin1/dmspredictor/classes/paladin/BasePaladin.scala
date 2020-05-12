package io.github.tjheslin1.dmspredictor.classes.paladin

import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.Armour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._

trait BasePaladin extends Player with SpellCaster with Product with Serializable {

  val fightingStyles: List[PaladinFightingStyle]

  def resetStartOfTurn(): Creature = this
}

object BasePaladin {

  val HitDice = D10

  def calculateHealth(level: Level, constitutionScore: Stat): Int =
    Player.calculateHealth(HitDice, level, constitutionScore)

  def weaponWithFightingStyle[_: RS](weapon: Weapon, fightingStyles: List[PaladinFightingStyle]): Weapon = ???

  def armourClassWithFightingStyle(stats: BaseStats, armour: Armour, offHand: Option[Equipment], fightingStyles: List[PaladinFightingStyle]) = ???

  // lenses...
}
