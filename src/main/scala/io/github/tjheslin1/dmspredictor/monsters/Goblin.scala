package io.github.tjheslin1.dmspredictor.monsters

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.monsters.Monster.defaultSavingThrowScores
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Goblin(health: Int,
                               maxHealth: Int,
                               stats: BaseStats = BaseStats(8, 14, 10, 10, 8, 8),
                               armourClass: Int = 15,
                               baseWeapon: Weapon = Shortsword,
                               armour: Armour = NoArmour,
                               offHand: Option[Equipment] = None,
                               resistances: List[DamageType] = List(),
                               immunities: List[DamageType] = List(),
                               conditions: List[Condition] = List.empty,
                               attackStatus: AttackStatus = Regular,
                               defenseStatus: AttackStatus = Regular,
                               name: String = NameGenerator.randomName)
    extends Monster {

  val challengeRating: Double                = 0.25
  val skills                                 = Skills(perception = 0, stealth = 6)
  val savingThrowScores: Map[Attribute, Int] = defaultSavingThrowScores(this)

  val creatureType: CreatureType = Humanoid

  val abilities: List[CombatantAbility] = List.empty

  val reactionUsed: Boolean = true

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Goblin =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = this
}

object Goblin {

  def calculateHealth[_: RS]: Int = 2 * D6

  def withName[_: RS](goblinName: String = NameGenerator.randomName): Goblin = {
    val hp = calculateHealth
    Goblin(hp, hp, name = goblinName)
  }

  implicit def goblinShow[_: RS]: Show[Goblin] = Show.show { goblin =>
    s"Goblin: " +
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
