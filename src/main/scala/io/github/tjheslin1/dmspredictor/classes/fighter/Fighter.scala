package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import cats.data.NonEmptyList
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter.standardFighterAbilities
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, ChainShirt, NoArmour}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
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
                                conditions: List[Condition] = List.empty,
                                attackStatus: AttackStatus = Regular,
                                defenseStatus: AttackStatus = Regular,
                                name: String = NameGenerator.randomName)
    extends BaseFighter {

  val savingThrowProficiencies = NonEmptyList.of(Strength, Constitution)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Fighter =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20
}

object Fighter {

  import BaseFighterAbilities._

  val HitDice = D10

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword, armour: Armour = ChainShirt): Fighter = {
    val health = calculateHealth(LevelOne, 14)
    Fighter(LevelOne, health, health, BaseStats(15, 13, 14, 12, 8, 10), weapon, armour)
  }

  val standardFighterAbilities: List[CombatantAbility] = List(
    actionSurge(1),
    secondWind(2),
    extraAttack(3),
    twoWeaponFighting(4)
  )

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
