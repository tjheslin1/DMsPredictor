package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.model.Weapon.weaponShow

sealed trait CreatureType

case object EnemyMonster    extends CreatureType
case object PlayerCharacter extends CreatureType

trait Creature {

  val creatureType: CreatureType

  val health: Int
  val maxHealth: Int
  val stats: BaseStats
  def weapon[_: RS]: Weapon
  val armour: Armour
  val offHand: Option[Equipment]
  val armourClass: Int
  val proficiencyBonus: Int
  val resistances: List[DamageType]
  val immunities: List[DamageType]
  val name: String

  val isConscious = health > 0

  def updateHealth(modification: Int): Creature

  val abilities: List[CreatureAbility]
}

object Creature {

  implicit val determineCritical: DetermineCritical[Creature] = new DetermineCritical[Creature] {
    def attackIsCritical(creature: Creature, roll: Int): Boolean = roll == 20
  }

  implicit def creatureShow[_: RS]: Show[Creature] = Show.show { creature =>
    s"${creature.creatureType} - " +
      s"Name: ${creature.name}, " +
      s"health: ${creature.health}, " +
      s"AC: ${creature.armourClass}, " +
      s"${creature.weapon.show}"
  }
}
