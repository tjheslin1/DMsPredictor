package io.github.tjheslin1.classes

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.model.Creature.creatureShow
import io.github.tjheslin1.model._
import io.github.tjheslin1.weapons.Greatsword

case class Fighter(creature: Creature)

object Fighter {

  def calculateHealth(implicit rollStrategy: RollStrategy): Int = D10.max

  def levelOneFighter(weapon: Weapon = Greatsword)(implicit rollStrategy: RollStrategy): Fighter =
    new Fighter(Creature(calculateHealth, BaseStats(15, 13, 14, 12, 8, 10), 14, 0, weapon, PlayerCharacter))

  implicit val fighterShow: Show[Fighter] = Show.show { fighter =>
    s"Fighter - ${fighter.creature.show}}"
  }
}
