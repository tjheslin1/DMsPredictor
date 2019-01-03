package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.FighterAbilities._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{ChainShirt, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.Weapon.bonusToHitWeapon
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.GenLens

case class Fighter(level: Level,
                   health: Int,
                   maxHealth: Int,
                   stats: BaseStats,
                   baseWeapon: Weapon,
                   armour: Armour = NoArmour,
                   offHand: Option[Equipment] = None,
                   fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
                   abilityUsages: FighterAbilities = allUnused(),
                   proficiencyBonus: Int = 0,
                   resistances: List[DamageType] = List(),
                   immunities: List[DamageType] = List(),
                   name: String = NameGenerator.randomName)
    extends Creature {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  def updateHealth(modification: Int): Fighter = copy(health = Math.max(0, health + modification))

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  val abilities: List[CreatureAbility] = fighterAbilities
}

object Fighter {

  import FighterAbilities._

  implicit val dc: DetermineCritical[Fighter] = DetermineCritical.default[Fighter]

  val HitDice = D10

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (D10.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(HitDice) + mod(
      constitutionScore)))

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword, armour: Armour = ChainShirt): Fighter = {
    val health = calculateHealth(LevelOne, 14)
    new Fighter(LevelOne, health, health, BaseStats(15, 13, 14, 12, 8, 10), weapon, armour)
  }

  val fighterAbilities: List[CreatureAbility] = List(
    1 -> secondWind,
    2 -> actionSurge,
    3 -> twoWeaponFighting,
    4 -> extraAttack
  )

  def weaponWithFightingStyle[_: RS](weapon: Weapon, fightingStyles: List[FighterFightingStyle]) =
    weapon.weaponType match {
      case Ranged if fightingStyles.contains(Archery) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded == false && fightingStyles.contains(Dueling) =>
        bonusToHitWeapon(weapon, 2)
      case Melee if weapon.twoHanded && fightingStyles.contains(GreatWeaponFighting) =>
        lazy val rerollingDamage = {
          val damageRoll = weapon.damage
          if (damageRoll <= 2)
            weapon.damage
          else
            damageRoll
        }
        Weapon(weapon.name, weapon.weaponType, weapon.damageType, weapon.twoHanded, rerollingDamage, weapon.hitBonus)
      case _ => weapon
    }

  def armourClassWithFightingStyle(stats: BaseStats,
                                   armour: Armour,
                                   offHand: Option[Equipment],
                                   fightingStyles: List[FighterFightingStyle]) = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus = offHand match {
      case Some(Shield()) => 2
      case _              => 0
    }
    val defenseBonus = if (fightingStyles.contains(Defense)) 1 else 0

    armour match {
      case NoArmour => baseArmourClass + shieldBonus
      case _        => baseArmourClass + shieldBonus + defenseBonus
    }
  }

  implicit def fighterShow[_: RS]: Show[Fighter] = Show.show { fighter =>
    s"Fighter: " +
      s"Name: ${fighter.name}, " +
      s"health: ${fighter.health}, " +
      s"AC: ${fighter.armourClass}, " +
      s"${fighter.weapon.show}"
  }

  val levelLens: Lens[Fighter, Level]                               = GenLens[Fighter](_.level)
  val healthLens: Lens[Fighter, Int]                                = GenLens[Fighter](_.health)
  val maxHealthLens: Lens[Fighter, Int]                             = GenLens[Fighter](_.maxHealth)
  val statLens: Lens[Fighter, BaseStats]                            = GenLens[Fighter](_.stats)
  val strengthLens: Lens[Fighter, Stat]                             = statLens composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Fighter, Stat]                            = statLens composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Fighter, Stat]                         = statLens composeLens GenLens[BaseStats](_.constitution)
  val baseWeaponLens: Lens[Fighter, Weapon]                         = GenLens[Fighter](_.baseWeapon)
  val armourLens: Lens[Fighter, Armour]                             = GenLens[Fighter](_.armour)
  val offHandLens: Lens[Fighter, Option[Equipment]]                 = GenLens[Fighter](_.offHand)
  val fightingStylesLens: Lens[Fighter, List[FighterFightingStyle]] = GenLens[Fighter](_.fightingStyles)
  val fighterAbilityUsagesLens: Lens[Fighter, FighterAbilities]     = GenLens[Fighter](_.abilityUsages)
  val proficiencyBonusLens: Lens[Fighter, Int]                      = GenLens[Fighter](_.proficiencyBonus)
  val resistancesLens: Lens[Fighter, List[DamageType]]              = GenLens[Fighter](_.resistances)
  val immunitiesLens: Lens[Fighter, List[DamageType]]               = GenLens[Fighter](_.immunities)
  val nameLens: Lens[Fighter, String]                               = GenLens[Fighter](_.name)
}

sealed trait FighterFightingStyle extends Product with Serializable

case object Archery             extends FighterFightingStyle
case object Defense             extends FighterFightingStyle
case object Dueling             extends FighterFightingStyle
case object GreatWeaponFighting extends FighterFightingStyle
case object Protection          extends FighterFightingStyle
case object TwoWeaponFighting   extends FighterFightingStyle
