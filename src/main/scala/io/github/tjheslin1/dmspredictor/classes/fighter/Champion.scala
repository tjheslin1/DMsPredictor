package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator

case class Champion(level: Level,
               health: Int,
               maxHealth: Int,
               stats: BaseStats,
               baseWeapon: Weapon,
               armour: Armour = NoArmour,
               offHand: Option[Equipment] = None,
               fightingStyles: List[FighterFightingStyle] = List.empty[FighterFightingStyle],
               abilities: FighterAbilities = FighterAbilities.allUnused(),
               override val proficiencyBonus: Int = 0,
               override val resistances: List[DamageType] = List(),
               override val immunities: List[DamageType] = List(),
               override val name: String = NameGenerator.randomName)
    extends Creature {

  import Fighter._

  val creatureType: CreatureType = PlayerCharacter

  def updateHealth(modification: Int): Champion = copy(health = Math.max(0, health + modification))

  def weapon[_: RS]: Weapon = weaponWithFightingStyle(baseWeapon, fightingStyles)

  def armourClass: Int = armourClassWithFightingStyle(stats, armour, offHand, fightingStyles)
}

object Champion {

  implicit val improvedCritical: DetermineCritical[Champion] = new DetermineCritical[Champion] {
    def attackIsCritical(roll: Int): Boolean = roll >= 19
  }
}
