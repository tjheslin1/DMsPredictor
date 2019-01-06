package util

import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
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
                                    proficiencyBonus: ProficiencyBonus      = 0,
                                    resistances: List[DamageType] = List(),
                                    immunities: List[DamageType] = List(),
                                    abilities: List[CreatureAbility] = List.empty,
                                    name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = EnemyMonster

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  def weapon[_: RS]: Weapon = baseWeapon
}

object TestMonster {

  val strengthLens: Lens[TestMonster, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestMonster, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
}
