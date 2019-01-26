package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter.standardFighterAbilities
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, ChainShirt, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model.Weapon.bonusToHitWeapon
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}

@Lenses("_") case class Fighter(level: Level,
                                health: Int,
                                maxHealth: Int,
                                stats: BaseStats,
                                baseWeapon: Weapon,
                                armour: Armour = NoArmour,
                                offHand: Option[Equipment] = None,
                                fightingStyles: List[FighterFightingStyle] =
                                  List.empty[FighterFightingStyle],
                                abilityUsages: BaseFighterAbilities = allUnused(),
                                proficiencyBonus: ProficiencyBonus = 0,
                                resistances: List[DamageType] = List.empty,
                                immunities: List[DamageType] = List.empty,
                                bonusActionUsed: Boolean = false,
                                abilities: List[CombatantAbility] = standardFighterAbilities,
                                name: String = NameGenerator.randomName)
    extends BaseFighter {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth(modification: Int): Fighter = copy(health = Math.max(0, health + modification))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object Fighter {

  import BaseFighterAbilities._

  val HitDice = D10

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (D10.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(HitDice) + mod(
      constitutionScore)))

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword, armour: Armour = ChainShirt): Fighter = {
    val health = calculateHealth(LevelOne, 14)
    new Fighter(LevelOne, health, health, BaseStats(15, 13, 14, 12, 8, 10), weapon, armour)
  }

  val standardFighterAbilities: List[CombatantAbility] = List(
    actionSurge(1),
    secondWind(2),
    extraAttack(3),
    twoWeaponFighting(4)
  )

  def weaponWithFightingStyle[_: RS](weapon: Weapon,
                                     fightingStyles: List[FighterFightingStyle]): Weapon =
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
        Weapon(weapon.name,
               weapon.weaponType,
               weapon.damageType,
               weapon.twoHanded,
               rerollingDamage,
               weapon.hitBonus)
      case _ => weapon
    }

  def armourClassWithFightingStyle(stats: BaseStats,
                                   armour: Armour,
                                   offHand: Option[Equipment],
                                   fightingStyles: List[FighterFightingStyle]): Int = {
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
      s"AC: ${fighter.armourClass}"
  }

  val strengthLens: Lens[Fighter, Stat]     = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[Fighter, Stat]    = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[Fighter, Stat] = _stats composeLens GenLens[BaseStats](_.constitution)
  val wisdomLens: Lens[Fighter, Stat]       = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[Fighter, Stat] = _stats composeLens GenLens[BaseStats](_.intelligence)
  val charismaLens: Lens[Fighter, Stat]     = _stats composeLens GenLens[BaseStats](_.charisma)
}
