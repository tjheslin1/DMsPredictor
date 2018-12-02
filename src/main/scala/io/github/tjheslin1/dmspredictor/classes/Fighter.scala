package io.github.tjheslin1.dmspredictor.classes

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureShow
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.fromLevel
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.weapons.Greatsword

case class Fighter(creature: Creature, level: Level)

object Fighter {

  def calculateHealth[_: RS]: Int = D10.max

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword): Fighter =
    new Fighter(
      Creature(calculateHealth, BaseStats(15, 13, 14, 12, 8, 10), 14, weapon, PlayerCharacter, fromLevel(LevelOne)),
      LevelOne)

  implicit val fighterShow: Show[Fighter] = Show.show { fighter =>
    s"Fighter - ${fighter.creature.show}}"
  }
}
