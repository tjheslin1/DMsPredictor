package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator

class Champion(level: Level,
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
    extends Fighter(level,
                    health,
                    maxHealth,
                    stats,
                    baseWeapon,
                    armour,
                    offHand,
                    fightingStyles,
                    abilities,
                    proficiencyBonus,
                    resistances,
                    immunities,
                    name) {}

object Champion {

  implicit val improvedCritical: DetermineCritical[Champion] = new DetermineCritical[Champion] {
    val message = "Champion improvedCritical"
    def attackIsCritical(roll: Int): Boolean = roll >= 19
  }
}
