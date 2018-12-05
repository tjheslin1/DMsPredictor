package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import io.github.tjheslin1.dmspredictor.model.Weapon.weaponShow

sealed trait CreatureType

case object Monster extends CreatureType

case object PlayerCharacter extends CreatureType

case class Creature(health: Int,
                    stats: BaseStats,
                    armourClass: Int,
                    weapon: Weapon,
                    creatureType: CreatureType,
                    proficiencyBonus: Int = 0,
                    resistances: List[DamageType] = List(),
                    immunities: List[DamageType] = List(),
                    name: String = NameGenerator.randomName) {

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
