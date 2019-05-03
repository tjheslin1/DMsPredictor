package io.github.tjheslin1.dmspredictor

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.{BaseWizard, Wizard}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf}
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val iterations = args(0).toInt

  val wizardHp = BaseWizard.calculateHealth(LevelFive, 14)

  println(s"???????ARGS: ${args.length}")
  args.foreach(println(_))

  val wizard = Wizard(
    LevelFive,
    wizardHp,
    wizardHp,
    BaseStats(10, 10, 14, 14, 14, 10),
    Shortsword,
    Skills(10, 10),
    Wizard.wizardSpellSlots(LevelFive),
    name = "Wizard"
  )

  val creatures = List(
    wizard,
    Goblin(50, 50, name = "goblin-1"),
    Goblin(50, 50, name = "goblin-2"),
    Werewolf(80, 80, name = "Werewolf")
  )

  val simulation     = "Wizard vs Goblins and Werewolf"
  val (losses, wins) = (0, 0)
  SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, iterations)

  logger.debug(s"$simulation simulation started")
  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show(title = simulation)
}
