package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.model.Weapon.weaponShow
import io.github.tjheslin1.dmspredictor.util.NameGenerator

sealed trait CreatureType

case object Monster extends CreatureType

case object PlayerCharacter extends CreatureType

trait Creature {

  def health: Int
  def stats: BaseStats
  def armourClass: Int
  def weapon: Weapon
  def creatureType: CreatureType
  def proficiencyBonus: Int = 0
  def resistances: List[DamageType] = List()
  def immunities: List[DamageType] = List()
  def name: String = NameGenerator.randomName

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
