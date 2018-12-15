package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import io.github.tjheslin1.dmspredictor.weapons.Shortsword

case class Goblin(health: Int,
                  stats: BaseStats,
                  armourClass: Int,
                  weapon: Weapon,
                  override val resistances: List[DamageType] = List(),
                  override val immunities: List[DamageType] = List(),
                  override val name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = Monster

  def updateHealth(modification: Int): Goblin = copy(health = Math.max(health + modification, 0))
}

object Goblin {

  def calculateHealth[_: RS]: Int = 2 * D6

  def levelOneGoblin[_: RS](weapon: Weapon = Shortsword): Goblin =
    Goblin(calculateHealth, BaseStats(8, 14, 10, 10, 8, 8), 15, weapon)

  implicit val goblinShow: Show[Goblin] = Show.show { goblin =>
    s"Fighter: " +
      s"Name: ${goblin.name}, " +
      s"health: ${goblin.health}, " +
      s"AC: ${goblin.armourClass}, " +
      s"${goblin.weapon.show}"
  }
}
