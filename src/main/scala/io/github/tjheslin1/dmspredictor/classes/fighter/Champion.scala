package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.Show
import cats.syntax.option._
import cats.syntax.show._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Prism
import monocle.macros.Lenses

@Lenses("_") case class Champion(level: Level,
                    health: Int,
                    maxHealth: Int,
                    stats: BaseStats,
                    baseWeapon: Weapon,
                    armour: Armour = NoArmour,
                    offHand: Option[Equipment] = None,
                    fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
                    abilityUsages: FighterAbilities = FighterAbilities.allUnused(),
                    proficiencyBonus: Int = 0,
                    resistances: List[DamageType] = List(),
                    immunities: List[DamageType] = List(),
                    name: String = NameGenerator.randomName)
    extends Creature {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  def updateHealth(modification: Int): Champion = copy(health = Math.max(0, health + modification))

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  val armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)

  val abilities: List[CreatureAbility] = championAbilities
}

object Champion {

  import FighterAbilities._

  val HitDice = D10

  implicit val improvedCritical: DetermineCritical[Champion] = new DetermineCritical[Champion] {
    def attackIsCritical(champion: Champion, roll: Int): Boolean =
      if (champion.level.value <= 2) roll == 20 else roll >= 19
  }

  implicit val championAbilities: List[CreatureAbility] = List(
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

  val prism: Prism[Creature, Champion] = Prism[Creature, Champion]{
    case c: Champion => c.some
    case _ => None
  }(identity)
}
