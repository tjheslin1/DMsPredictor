package io.github.tjheslin1.simulation

import io.github.tjheslin1.model.{Creature, PlayerCharacter}

case class BasicSimulation(pcs: List[PlayerCharacter], mobs: List[Creature]) extends Simulation {

  def characters: List[PlayerCharacter] = pcs
  def monsters: List[Creature] = mobs

  def run: SimulationResult = {
    characters match {
      case character :: Nil => SimulationResult(character.attack(monsters.head), "Fighter vs Goblin")
      case _ => SimulationResult(Unknown, "BasicSimulation only handles a single character vs a single monster")
    }
  }
}
