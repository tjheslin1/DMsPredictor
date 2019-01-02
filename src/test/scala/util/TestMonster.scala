package util

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.GenLens

case class TestMonster(health: Int,
                       maxHealth: Int,
                       stats: BaseStats,
                       armourClass: Int,
                       wpn: Weapon,
                       abilities: List[CreatureAbility] = List.empty,
                       resistances: List[DamageType] = List(),
                       immunities: List[DamageType] = List(),
                       name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = EnemyMonster
  val proficiencyBonus: Int      = 0

  def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

  def weapon[_: RS]: Weapon = wpn

  val armour: Armour             = NoArmour
  val offHand: Option[Equipment] = none[Equipment]
}

object TestMonster {
  val healthLens: Lens[TestMonster, Int]                                = GenLens[TestMonster](_.health)
  val maxHealthLens: Lens[TestMonster, Int]                             = GenLens[TestMonster](_.maxHealth)
  val statLens: Lens[TestMonster, BaseStats]                            = GenLens[TestMonster](_.stats)
  val strengthLens: Lens[TestMonster, Stat]                             = statLens composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestMonster, Stat]                            = statLens composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestMonster, Stat]                         = statLens composeLens GenLens[BaseStats](_.constitution)
  val baseWeaponLens: Lens[TestMonster, Weapon]                         = GenLens[TestMonster](_.wpn)
  val armourLens: Lens[TestMonster, Armour]                             = GenLens[TestMonster](_.armour)
  val offHandLens: Lens[TestMonster, Option[Equipment]]                 = GenLens[TestMonster](_.offHand)
  val proficiencyBonusLens: Lens[TestMonster, Int]                      = GenLens[TestMonster](_.proficiencyBonus)
  val resistancesLens: Lens[TestMonster, List[DamageType]]              = GenLens[TestMonster](_.resistances)
  val immunitiesLens: Lens[TestMonster, List[DamageType]]               = GenLens[TestMonster](_.immunities)
  val nameLens: Lens[TestMonster, String]                               = GenLens[TestMonster](_.name)
}