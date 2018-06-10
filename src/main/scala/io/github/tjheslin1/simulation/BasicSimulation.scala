package io.github.tjheslin1.simulation

import io.github.tjheslin1.model.{Creature, PlayerCharacter, RollStrategy}

case class BasicSimulation(characters: List[PlayerCharacter], monsters: List[Creature]) extends Simulation {

  def pcs: List[PlayerCharacter] = characters

  def mobs: List[Creature] = monsters

  def run(implicit rollStrategy: RollStrategy): SimulationResult = {
    pcs match {
      case character :: Nil =>
        if (character.attack(mobs.head) == Success)
          SimulationResult(character.resolveDamage(mobs.head), "Fighter vs Goblin")
        else SimulationResult(Loss, "Fighter vs Goblin")
      case _ =>
        SimulationResult(Unknown, "BasicSimulation only handles a single character vs a single monster")
    }
  }

}
