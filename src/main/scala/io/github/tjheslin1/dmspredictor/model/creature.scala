package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.model.Weapon.weaponShow
import io.github.tjheslin1.dmspredictor.util.NameGenerator

sealed trait CreatureType

case object Monster extends CreatureType

case object PlayerCharacter extends CreatureType

trait Creature {

  val creatureType: CreatureType

  val health: Int
  val stats: BaseStats
  val weapon: Weapon
  def armourClass: Int
  def proficiencyBonus: Int         = 0
  def resistances: List[DamageType] = List()
  def immunities: List[DamageType]  = List()
  def name: String                  = NameGenerator.randomName

  val isConscious = health > 0

  def updateHealth(modification: Int): Creature
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
