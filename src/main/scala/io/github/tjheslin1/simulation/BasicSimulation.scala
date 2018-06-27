package io.github.tjheslin1.simulation

import io.github.tjheslin1.model._

import scala.collection.mutable

case class BasicSimulation(cs: List[Creature]) extends Simulation {

  val creatures = cs

  def run(info: String)(implicit rollStrategy: RollStrategy): SimulationResult = {

    def determineOutcome(initv: Map[String, Initiative], pcs: List[Creature], mobs: List[Creature]): SimulationResult =
      if (pcs.exists(_.health > 0)) {
        if (mobs.exists(_.health > 0)) {

          val (pcs, mobs) = Turn(initv).run.toList.partition(_.creatureType == PlayerCharacter)

          val updatedInitiative = mutable.Map[String, Initiative]()
          pcs.foreach(pc => updatedInitiative.put(pc.name, initv(pc.name)))
          mobs.foreach(mob => updatedInitiative.put(mob.name, initv(mob.name)))

          determineOutcome(initv, pcs, mobs)
        } else SimulationResult(Success, info)
      } else SimulationResult(Loss, info)

    val initiative = InitiativeCalculator(creatures).rollInitiative

    val (playerCharacters, monsters) = initiative.toList.map(_._2.creature).partition(_.creatureType == PlayerCharacter)

    determineOutcome(initiative, playerCharacters, monsters)
  }
}
