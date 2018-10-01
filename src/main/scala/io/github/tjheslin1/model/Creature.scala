package io.github.tjheslin1.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.util.NameGenerator
import io.github.tjheslin1.model.Weapon.weaponShow

sealed trait CreatureType

case object Monster extends CreatureType

case object PlayerCharacter extends CreatureType

case class Creature(health: Int,
                    stats: BaseStats,
                    armourClass: Int,
                    experience: Int,
                    weapon: Weapon,
                    creatureType: CreatureType,
                    name: String = NameGenerator.randomName) {

  val proficiencyBonus = 2

  def isConscious = health > 0
}

object Creature {

  implicit val creatureShow: Show[Creature] = Show.show { creature =>
    s"${creature.creatureType} - " +
      s"Name: ${creature.name}, " +
      s"health: ${creature.health}, " +
      s"AC: ${creature.armourClass}, " +
      s"${creature.weapon.show}"
  }
}
