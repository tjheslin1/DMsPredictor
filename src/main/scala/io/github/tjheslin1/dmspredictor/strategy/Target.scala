package io.github.tjheslin1.dmspredictor.strategy

import io.github.tjheslin1.dmspredictor.model._

object Target {

  def monsters(combatants: List[Combatant]): List[Combatant] =
    combatants.filter {
      case c: Combatant if c.creature.creatureType == PlayerCharacter => false
      case _                                                          => true
    }

  def players(combatants: List[Combatant]): List[Combatant] =
    combatants.filter {
      case c: Combatant if c.creature.creatureType == PlayerCharacter => true
      case _                                                          => false
    }
}
