package io.github.tjheslin1.dmspredictor

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.{BaseRogue, Rogue}
import io.github.tjheslin1.dmspredictor.equipment.armour.NoArmour
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val rogueHp   = BaseRogue.calculateHealth(LevelTwo, 14)
  val profBonus = ProficiencyBonus.fromLevel(LevelTwo)

  val rogue = Rogue(
    LevelTwo,
    rogueHp,
    rogueHp,
    BaseStats(10, 14, 14, 10, 14, 10),
    Shortsword,
    Skills(perception = profBonus, stealth = profBonus),
    NoArmour,
    proficiencyBonus = profBonus,
    name = "Rogue"
  )

  val creatures = List(
    rogue,
    Goblin.levelOneGoblin(goblinName = "goblin-1"),
    Goblin.levelOneGoblin(goblinName = "goblin-2")
  )

  val simulation = "Rogue vs Goblins"
  val (losses, wins) =
    SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, 1)

  logger.debug(s"$simulation simulation started")
  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show(title = simulation)
}
