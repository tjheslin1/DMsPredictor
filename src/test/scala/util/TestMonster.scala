package util

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class TestMonster(health: Int,
                                    maxHealth: Int,
                                    stats: BaseStats,
                                    armourClass: Int,
                                    baseWeapon: Weapon,
                                    abilities: List[CreatureAbility] = List.empty,
                                    resistances: List[DamageType] = List(),
                                    immunities: List[DamageType] = List(),
                                    name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = EnemyMonster
  val proficiencyBonus: Int      = 0

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  def weapon[_: RS]: Weapon = baseWeapon

  val armour: Armour             = NoArmour
  val offHand: Option[Equipment] = none[Equipment]
}

object TestMonster {

  val strengthLens: Lens[TestMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestMonster, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
}
