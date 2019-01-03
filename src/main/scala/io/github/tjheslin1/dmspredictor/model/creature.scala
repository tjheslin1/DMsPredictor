package io.github.tjheslin1.dmspredictor.model

import cats.Show
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.monsters._
import monocle.Lens

sealed trait CreatureType

case object EnemyMonster    extends CreatureType
case object PlayerCharacter extends CreatureType

trait Creature {

  val creatureType: CreatureType

  val health: Int
  val maxHealth: Int
  val stats: BaseStats
  val baseWeapon: Weapon
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

  // TODO
  val creatureHealthLens: Lens[Creature, Int] = Lens[Creature, Int](_.health) { hp =>
    {
      case c: Champion => Champion._health.set(hp)(c)
      case c: Fighter  => Fighter._health.set(hp)(c)

      case c: Goblin  => Goblin._health.set(hp)(c)
      case c: Werewolf  => Werewolf._health.set(hp)(c)
    }
  }

  implicit val determineCritical: DetermineCritical[Creature] = new DetermineCritical[Creature] {
    def attackIsCritical(creature: Creature, roll: Int): Boolean = roll == 20
  }

  implicit def creatureShow[_: RS]: Show[Creature] = Show.show { creature =>
    s"${creature.creatureType} - " +
      s"Name: ${creature.name}, " +
      s"health: ${creature.health}, " +
      s"AC: ${creature.armourClass}"
  }
}
