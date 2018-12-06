package io.github.tjheslin1.dmspredictor.classes

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureShow
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.fromLevel
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import io.github.tjheslin1.dmspredictor.weapons.Greatsword

case class Fighter(level: Level,
                   health: Int,
                   stats: BaseStats,
                   armourClass: Int,
                   weapon: Weapon,
                   override val resistances: List[DamageType] = List(),
                   override val immunities: List[DamageType] = List(),
                   override val name: String = NameGenerator.randomName)
    extends Creature {

  def creatureType: CreatureType = PlayerCharacter
}

object Fighter {

  def calculateHealth[_: RS]: Int = D10.max

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword): Fighter =
    new Fighter(LevelOne, calculateHealth, BaseStats(15, 13, 14, 12, 8, 10), 14, weapon)

  implicit val fighterShow: Show[Fighter] = Show.show { fighter =>
    s"Fighter: " +
      s"Name: ${fighter.name}, " +
      s"health: ${fighter.health}, " +
      s"AC: ${fighter.armourClass}, " +
      s"${fighter.weapon.show}"
  }
}
