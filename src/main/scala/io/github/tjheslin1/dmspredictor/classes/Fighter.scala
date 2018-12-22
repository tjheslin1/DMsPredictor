package io.github.tjheslin1.dmspredictor.classes

import cats.Show
import cats.syntax.show._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.equipment.armour.{ChainShirt, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.{Ability, ClassAbilities}
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.NameGenerator

case class Fighter(level: Level,
                   health: Int,
                   maxHealth: Int,
                   stats: BaseStats,
                   baseWeapon: Weapon,
                   armour: Armour = NoArmour,
                   shield: Option[Shield] = None,
                   fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
                   secondWindUsed: Boolean = false,
                   override val proficiencyBonus: Int = 0,
                   override val resistances: List[DamageType] = List(),
                   override val immunities: List[DamageType] = List(),
                   override val name: String = NameGenerator.randomName)
    extends Creature {

  val creatureType: CreatureType = PlayerCharacter

  def updateHealth(modification: Int): Fighter = copy(health = Math.max(0, health + modification))

  def armourClass: Int = {
    val baseArmourClass = armour.armourClass(stats.dexterity)
    val shieldBonus     = shield.fold(0)(_.armourClass(stats.dexterity))
    val defenseBonus    = if (fightingStyles.contains(Defense)) 1 else 0

    armour match {
      case NoArmour  => baseArmourClass + shieldBonus
      case _: Armour => baseArmourClass + shieldBonus + defenseBonus
    }
  }

  def weapon[_: RS]: Weapon = baseWeapon.weaponType match {
    case Melee =>
      if (fightingStyles.contains(Dueling))
        Weapon(baseWeapon.name, baseWeapon.weaponType, baseWeapon.damageType, baseWeapon.damage, wpnHitBonus = 2)
      else baseWeapon
    case Ranged =>
      if (fightingStyles.contains(Archery))
        Weapon(baseWeapon.name, baseWeapon.weaponType, baseWeapon.damageType, baseWeapon.damage, wpnHitBonus = 2)
      else baseWeapon
  }
}

object Fighter {

  def calculateHealth[_: RS](level: Level, constitutionScore: Stat): Int =
    (D10.max + mod(constitutionScore)) + ((level.value - 1) * (Dice.midpointRoundedUp(D10) + mod(constitutionScore)))

  def levelOneFighter[_: RS](weapon: Weapon = Greatsword, armour: Armour = ChainShirt): Fighter = {
    val health = calculateHealth(LevelOne, 14)
    new Fighter(LevelOne, health, health, BaseStats(15, 13, 14, 12, 8, 10), weapon, armour)
  }

  implicit val fighterAbilities = new ClassAbilities[Fighter] {
    def abilities: List[(Int, Fighter => Ability[Fighter])] = List(1 -> secondWind _)
  }

  def secondWind(fighter: Fighter): Ability[Fighter] = new Ability[Fighter](fighter) {
    val levelRequirement = LevelTwo
    val triggerMet       = fighter.health <= fighter.maxHealth / 2
    val conditionMet     = fighter.level.value >= levelRequirement && fighter.secondWindUsed == false

    def useAbility[_: RS]: Fighter =
      fighter.copy(health = Math.min(fighter.maxHealth, fighter.health + 1 * D10 + fighter.level.value))

    def update: Fighter = fighter.copy(secondWindUsed = true)
  }

  implicit def fighterShow[_: RS]: Show[Fighter] = Show.show { fighter =>
    s"Fighter: " +
      s"Name: ${fighter.name}, " +
      s"health: ${fighter.health}, " +
      s"AC: ${fighter.armourClass}, " +
      s"${fighter.weapon.show}"
  }
}

sealed trait FighterFightingStyle extends Product with Serializable

case object Archery             extends FighterFightingStyle
case object Defense             extends FighterFightingStyle
case object Dueling             extends FighterFightingStyle
case object GreatWeaponFighting extends FighterFightingStyle
case object Protection          extends FighterFightingStyle
case object TwoWeaponFighting   extends FighterFightingStyle
