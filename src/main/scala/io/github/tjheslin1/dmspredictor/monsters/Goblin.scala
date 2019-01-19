package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Goblin(health: Int,
                               maxHealth: Int,
                               stats: BaseStats,
                               armourClass: Int,
                               baseWeapon: Weapon = Shortsword,
                               armour: Armour = NoArmour,
                               offHand: Option[Equipment] = None,
                               resistances: List[DamageType] = List(),
                               immunities: List[DamageType] = List(),
                               abilities: List[CreatureAbility] = List.empty,
                               name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType         = Monster
  val proficiencyBonus: ProficiencyBonus = 0

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth(modification: Int): Goblin = copy(health = Math.max(health + modification, 0))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object Goblin {

  def calculateHealth[_: RS]: Int = 2 * D6

  def levelOneGoblin[_: RS](): Goblin = {
    val hp = calculateHealth
    Goblin(hp, hp, BaseStats(8, 14, 10, 10, 8, 8), 15)
  }

  implicit def goblinShow[_: RS]: Show[Goblin] = Show.show { goblin =>
    s"Fighter: " +
      s"Name: ${goblin.name}, " +
      s"health: ${goblin.health}, " +
      s"AC: ${goblin.armourClass}"
  }

  val strengthLens: Lens[Goblin, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Goblin, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Goblin, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Goblin, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Goblin, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Goblin, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
