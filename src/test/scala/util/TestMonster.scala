package util

import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class TestMonster(health: Int,
                                    maxHealth: Int,
                                    stats: BaseStats,
                                    armourClass: Int,
                                    baseWeapon: Weapon,
                                    armour: Armour = NoArmour,
                                    offHand: Option[Equipment] = none[Equipment],
                                    proficiencyBonus: ProficiencyBonus = 0,
                                    resistances: List[DamageType] = List.empty,
                                    immunities: List[DamageType] = List.empty,
                                    abilities: List[CombatantAbility] = List.empty,
                                    conditions: List[Condition] = List.empty,
                                    attackStatus: AttackStatus = Regular,
                                    defenseStatus: AttackStatus = Regular,
                                    turnResetTracker: Unit => Unit = () => _,
                                    name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = Monster

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn[_: RS](): Creature = {
    turnResetTracker()
    this
  }
}

object TestMonster {

  val strengthLens: Lens[TestMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestMonster, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[TestMonster, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[TestMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
