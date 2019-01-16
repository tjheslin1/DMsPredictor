package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Champion(level: Level,
                                 health: Int,
                                 maxHealth: Int,
                                 stats: BaseStats,
                                 baseWeapon: Weapon,
                                 armour: Armour = NoArmour,
                                 offHand: Option[Equipment] = None,
                                 fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
                                 abilityUsages: FighterAbilities = FighterAbilities.allUnused(),
                                 proficiencyBonus: ProficiencyBonus = 0,
                                 resistances: List[DamageType] = List.empty,
                                 immunities: List[DamageType] = List.empty,
                                 abilities: List[CreatureAbility] = standardChampionAbilities,
                                 name: String = NameGenerator.randomName)
    extends Creature {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth(modification: Int): Champion = copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = if (level.value <= 2) roll == 20 else roll >= 19
}

object Champion {

  import FighterAbilities._

  val HitDice = D10

  implicit val standardChampionAbilities: List[CreatureAbility] = List(
    1 -> secondWind,
    2 -> actionSurge,
    3 -> twoWeaponFighting,
    4 -> extraAttack
  )

  implicit def championShow[_: RS]: Show[Champion] = Show.show { champion =>
    s"Fighter: " +
      s"Name: ${champion.name}, " +
      s"health: ${champion.health}, " +
      s"AC: ${champion.armourClass}"
  }

  val strengthLens: Lens[Champion, Stat]     = Champion._stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Champion, Stat]    = Champion._stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Champion, Stat] = Champion._stats composeLens GenLens[BaseStats](_.constitution)
}
