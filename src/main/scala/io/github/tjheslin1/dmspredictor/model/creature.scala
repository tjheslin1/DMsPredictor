package io.github.tjheslin1.dmspredictor.model

import cats.Show
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.model.Weapon.weaponShow
import io.github.tjheslin1.dmspredictor.strategy.ClassAbilities
import io.github.tjheslin1.dmspredictor.util.NameGenerator

sealed trait CreatureType

case object EnemyMonster    extends CreatureType
case object PlayerCharacter extends CreatureType

trait Creature {

  val creatureType: CreatureType

  val health: Int
  val stats: BaseStats
  def weapon[_: RS]: Weapon
  def armourClass: Int
  def proficiencyBonus: Int         = 0
  def resistances: List[DamageType] = List()
  def immunities: List[DamageType]  = List()
  def name: String                  = NameGenerator.randomName

  val isConscious = health > 0

  def updateHealth(modification: Int): Creature
}

object Creature {

  implicit val creatureAbilities: ClassAbilities[Creature] = new ClassAbilities[Creature] {
    def abilities: List[CreatureAbility[Creature]] = List.empty[CreatureAbility[Creature]]
  }

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
