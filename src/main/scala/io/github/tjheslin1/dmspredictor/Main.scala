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

case class SQSMessage(Records: Seq[SQSRecord])

case class SQSRecord(body: String, messageAttributes: MessageAttributes)

case class MessageAttributes(simulationHash: SimulationHash)

case class SimulationHash(stringValue: String)

case class SimulationConfig(
    simulationName: String,
    simulations: Int,
    focus: String,
    players: List[Player],
    monsters: List[Monster]
)

class Main extends RequestStreamHandler with ArgParser with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  override def handleRequest(request: InputStream, output: OutputStream, context: Context): Unit = {

    val input = scala.io.Source.fromInputStream(request).mkString

    val config: Either[Error, (SimulationConfig, String, BasicSimulation)] = parseSimulation(input)

    val (wins, losses) = config match {
      case Left(e) =>
        throw new RuntimeException(s"Error parsing JSON\\n$input\\n${e.getMessage}", e)
      case Right((simulationConfig, simHash, basicSimulation)) =>
        val (losses, wins) =
          SimulationRunner.run(
            basicSimulation,
            simulationConfig.simulationName,
            Math.min(10000, simulationConfig.simulations)
          )

        logger.debug(s"${simulationConfig.simulationName} simulation started - $simHash")
        println(s"$wins Wins and $losses Losses")

//        val data  = Seq("wins" -> wins, "losses" -> losses)
//        val chart = BarChart(data)
//        chart.show(title = simulationConfig.simulationName)

        (wins, losses)
    }

    import org.scanamo._
    import org.scanamo.syntax._
    import org.scanamo.auto._

    val client = DynamoDB.client()


    output.write(s"""{"wins":$wins,"losses":$losses}""".getBytes("UTF-8"))
  }

  def parseSimulation(input: String): Either[Error, (SimulationConfig, String, BasicSimulation)] =
    for {
      sqsMessage    <- decode[SQSMessage](input)
      message       = sqsMessage.Records.head
      configuration <- decode[SimulationConfig](message.body)
      simHash       = message.messageAttributes.simulationHash.stringValue
      parsedFocus   <- parseFocus(configuration.focus)
    } yield (
      configuration,
      simHash,
      BasicSimulation(configuration.players ++ configuration.monsters, parsedFocus)
    )
}
