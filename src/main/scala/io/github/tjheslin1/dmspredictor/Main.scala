package io.github.tjheslin1.dmspredictor

import java.io.{InputStream, OutputStream}

import com.amazonaws.services.lambda.runtime.{Context, RequestStreamHandler}
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}

case class SimulationConfig(simulationName: String,
                            simulations: Int,
                            focus: String,
                            players: List[Player],
                            monsters: List[Monster])

class Main extends RequestStreamHandler with ArgParser with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  override def handleRequest(request: InputStream, output: OutputStream, context: Context): Unit = {

    val input = scala.io.Source.fromInputStream(request).mkString

    val config: Either[Error, (SimulationConfig, BasicSimulation)] = for {
      configuration <- decode[SimulationConfig](input)
      parsedFocus   <- parseFocus(configuration.focus)
    } yield
      (configuration, BasicSimulation(configuration.players ++ configuration.monsters, parsedFocus))

    val (wins, losses) = config match {
      case Left(e) =>
        throw new RuntimeException(s"Error parsing JSON\\n$input\\n${e.getMessage}", e)
      case Right((simulationConfig, basicSimulation)) =>
        val (losses, wins) =
          SimulationRunner.run(basicSimulation,
                               simulationConfig.simulationName,
                               Math.min(10000, simulationConfig.simulations))

        logger.debug(s"${simulationConfig.simulationName} simulation started")
        println(s"$wins Wins and $losses Losses")

        //      val data  = Seq("wins" -> wins, "losses" -> losses)
        //      val chart = BarChart(data)
        //      chart.show(title = simulationConfig.simulationName)

        (wins, losses)
    }

    output.write(s"""{"wins":$wins,"losses":$losses}""".getBytes("UTF-8"))
  }
}
