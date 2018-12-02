package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureShow
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.weapons.Shortsword

case class Goblin(creature: Creature)

object Goblin {

  def calculateHealth[_: RS]: Int = 2 * D6

  def levelOneGoblin[_: RS](weapon: Weapon = Shortsword) =
    Goblin(Creature(calculateHealth, BaseStats(8, 14, 10, 10, 8, 8), 15, weapon, Monster))

  implicit val goblinShow: Show[Goblin] = Show.show { goblin =>
    s"Goblin - ${goblin.creature.show}}"
  }
}
