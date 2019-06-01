package io.github.tjheslin1.dmspredictor

import cats.data.NonEmptyList
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

case class SimulationConfig(simulationName: String,
                            simulations: Int,
                            focus: String,
                            players: NonEmptyList[Player],
                            monsters: NonEmptyList[Monster])

object Main extends App with ArgParser with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val config: Either[Error, (SimulationConfig, BasicSimulation)] = for {
    configuration      <- decode[SimulationConfig](args(0))
    parsedFocus <- parseFocus(configuration.focus)
  } yield (configuration, BasicSimulation(configuration.players.toList ++ configuration.monsters.toList, parsedFocus))

  val (wins, losses) = config match {
    case Left(e) => throw new RuntimeException(s"Error parsing JSON\n${e.getMessage}", e)
    case Right((simulationConfig, basicSimulation)) =>
      val (losses, wins) =
        SimulationRunner.run(basicSimulation, simulationConfig.simulationName, Math.max(10000, simulationConfig.simulations))

      logger.debug(s"${simulationConfig.simulationName} simulation started")
      println(s"$wins Wins and $losses Losses")

//      val data  = Seq("wins" -> wins, "losses" -> losses)
//      val chart = BarChart(data)
//      chart.show(title = simulationConfig.simulationName)

      (wins, losses)
  }

  def parseFocus(focus: String): Either[Error, Focus] = focus.toLowerCase match {
    case "lowestfirst" => LowestFirst.asRight
    case "randomfocus" => RandomFocus.asRight
    case _             => Left(ParsingFailure(s"unknown focus strategy provided: $focus", null))
  }

  s"$wins,$losses"
}
