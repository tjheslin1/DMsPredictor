package io.github.tjheslin1.dmspredictor.model

import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat

object Modifier {

  def mod(stat: Stat): Int = modifier(stat.value)
  def mod(score: Int): Int = modifier(score)

  // format: off
  def attributeModifier(creature: Creature, attribute: Attribute): Int =
    attribute match {
      case Strength     => mod(creature.stats.strength)
      case Dexterity    => mod(creature.stats.dexterity)
      case Constitution => mod(creature.stats.constitution)
      case Wisdom       => mod(creature.stats.wisdom)
      case Intelligence => mod(creature.stats.intelligence)
      case Charisma     => mod(creature.stats.charisma)
    }
  // format: on

  // format: off
  private val modifier = Map(
    1  -> -5,   10 -> 0,
    2  -> -4,   11 -> 0,
    3  -> -4,   12 -> 1,
    4  -> -3,   13 -> 1,
    5  -> -3,   14 -> 2,
    6  -> -2,   15 -> 2,
    7  -> -2,   16 -> 3,
    8  -> -1,   17 -> 3,
    9  -> -1,   18 -> 4,
                19 -> 4,

    20 -> 5,
    21 -> 5,
    22 -> 6,
    23 -> 6,
    24 -> 7,
    25 -> 7,
    26 -> 8,
    27 -> 8,
    28 -> 9,
    29 -> 9,
    30 -> 10
  )
}
