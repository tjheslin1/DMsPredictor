package io.github.tjheslin1.dmspredictor

import cats.data.NonEmptyList
import cats.syntax.either._
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.circe._
import io.circe.parser.decode
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.wizard.{BaseWizard, Wizard}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Monster, Werewolf}
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

/*
{
 "simulationName": "Wizard vs Goblins",
 "simulations": 2,
 "focus": "LowestFirst",
 "players": [
   {
    "level": 4,
    "stats": "10,10,14,14,14,10",
    "weapon": "Shortsword",
    "skills": "1,1",
    "name": "TestWizard"
   }
 ],
 "monsters": [
  {
    "name": "TestGoblin"
  }
 ]
}
 */

case class SimulationConfig(simulationName: String, simulations: Int, focus: String, players: NonEmptyList[Player], monsters: NonEmptyList[Monster])

object Main extends App with ArgParser with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val x: Either[Error, (SimulationConfig, BasicSimulation)] = for {
    config <- decode[SimulationConfig](args(0))
    parsedFocus <- parseFocus(config.focus)
  } yield
    (config, BasicSimulation(config.players.toList ++ config.monsters.toList, parsedFocus))

  x match {
    case Left(e) => throw new RuntimeException(s"Error parsing JSON\n${e.getMessage}", e)
    case Right((config, basicSimulation)) =>
      val (losses, wins) = SimulationRunner.run(basicSimulation, config.simulationName, config.simulations)

      logger.debug(s"${config.simulationName} simulation started")
      println(s"$wins Wins and $losses Losses")

      val data  = Seq("wins" -> wins, "losses" -> losses)
      val chart = BarChart(data)
      chart.show(title = config.simulationName)
  }

  val test =
    """
      |{
      | "simulationName": "Wizard vs Goblins",
      |
      |}
    """.stripMargin

//  val wizardHp = BaseWizard.calculateHealth(LevelFive, 14)
//
//  println(s"???????ARGS: ${args.length}")
//  args.foreach(println(_))
//
//  val wizard = Wizard(
//    LevelFive,
//    wizardHp,
//    wizardHp,
//    BaseStats(10, 10, 14, 14, 14, 10),
//    Shortsword,
//    Skills(10, 10),
//    Wizard.wizardSpellSlots(LevelFive),
//    name = "Wizard"
//  )
//
//  val creatures = List(
//    wizard,
//    Goblin(50, 50, name = "goblin-1"),
//    Goblin(50, 50, name = "goblin-2"),
//    Werewolf(80, 80, name = "Werewolf")
//  )

//  val simulation     = "Wizard vs Goblins and Werewolf"
//  val (losses, wins) = (0, 0)
//  SimulationRunner.run(basicSimulation, simulation, iterations)
//
//  logger.debug(s"$simulation simulation started")
//  println(s"$wins Wins and $losses Losses")
//
//  val data  = Seq("wins" -> wins, "losses" -> losses)
//  val chart = BarChart(data)
//  chart.show(title = simulation)

  def parseFocus(focus: String): Either[Error, Focus] = focus.toLowerCase match {
    case "lowestfirst" => LowestFirst.asRight
    case "randomfocus" => RandomFocus.asRight
    case _ => Left(ParsingFailure(s"unknown focus strategy provided: $focus", null))
  }
}
