package io.github.tjheslin1.model

import io.github.tjheslin1.util.NameGenerator

sealed trait CreatureType

case object Monster extends CreatureType

case object PlayerCharacter extends CreatureType

case class Creature(health: Int,
                    stats: BaseStats,
                    armourClass: Int,
                    experience: Int,
                    weapon: Weapon,
                    creatureType: CreatureType,
                    name: String = NameGenerator.randomName) {

  val proficiencyBonus = 2
}