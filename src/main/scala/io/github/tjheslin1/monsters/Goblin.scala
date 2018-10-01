package io.github.tjheslin1.monsters

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.model.Creature.creatureShow
import io.github.tjheslin1.model.{BaseStats, Creature, D6, Monster, RollStrategy, Weapon}
import io.github.tjheslin1.util.IntOps._
import io.github.tjheslin1.weapons.Shortsword

case class Goblin(creature: Creature)

object Goblin {

  def calculateHealth(implicit rollStrategy: RollStrategy): Int = 2 * D6

  def levelOneGoblin(weapon: Weapon = Shortsword)(implicit rollStrategy: RollStrategy) =
    Goblin(Creature(calculateHealth, BaseStats(8, 14, 10, 10, 8, 8), 15, 0, weapon, Monster))

  implicit val goblinShow: Show[Goblin] = Show.show { goblin =>
    s"Goblin - ${goblin.creature.show}}"
  }
}
