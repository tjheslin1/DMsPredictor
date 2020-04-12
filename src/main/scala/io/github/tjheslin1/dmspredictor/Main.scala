package io.github.tjheslin1.dmspredictor

import java.io.{InputStream, OutputStream}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.lambda.runtime.{Context, RequestStreamHandler}
import com.gu.scanamo.{Scanamo, Table}
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter.{BaseFighter, Champion}
import io.github.tjheslin1.dmspredictor.equipment.armour.ChainShirt
import io.github.tjheslin1.dmspredictor.equipment.weapons.{Greatsword, Shortsword}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst

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

case class SimulationNameField(simulationName: String)

case class SimulationResult(sim_hash: String, sim_name: String, result: String, config: String)

//object Main extends App with LazyLogging {
//
//  implicit val rollStrategy = Dice.defaultRandomiser
//
//  import eu.timepit.refined.auto._
//
//  val championStats = BaseStats(16, 15, 13, 12, 10, 9)
//  val championLevel = LevelFive
//  val championHp    = BaseFighter.calculateHealth(championLevel, championStats.constitution)
//  val champion = Champion(championLevel,
//                          championHp,
//                          championHp,
//                          championStats,
//                          Greatsword,
//                          Skills(1, 2),
//                          ChainShirt,
//                          name = "Champion")
//
//  val clericStats = BaseStats(9, 10, 13, 16, 10, 15)
//  val clericLevel = LevelFive
//  val clericHp    = BaseCleric.calculateHealth(clericLevel, clericStats.constitution)
//  val cleric = Cleric(clericLevel,
//                      clericHp,
//                      clericHp,
//                      clericStats,
//                      Shortsword,
//                      Skills(2, 1),
//                      Cleric.clericSpellSlots(clericLevel),
//                      armour = ChainShirt,
//                      name = "Cleric")
//
//  val lichHp = Lich.calculateHealth()
//  val lich = Lich(lichHp, lichHp, name = "Lich")
//
//  SimulationRunner.run(BasicSimulation(List(champion, cleric, lich), LowestFirst), "", 1)
//}

class Main extends RequestStreamHandler with ArgParser with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  override def handleRequest(request: InputStream, output: OutputStream, context: Context): Unit = {

    val input = scala.io.Source.fromInputStream(request).mkString

    val config: Either[Error, (SimulationConfig, String, BasicSimulation, String)] =
      parseSimulation(input)

    config match {
      case Left(e) =>
        parseSimulationHash(input) match {
          case Left(nameFieldError) =>
            val dateTime = LocalDateTime.now().format(DateTimeFormatter.BASIC_ISO_DATE)

            writeToDynamo(
              SimulationResult(dateTime, "unknown", nameFieldError.getMessage, "unknown config")
            )
          case Right(simHash) =>
            writeToDynamo(SimulationResult(simHash, "unknown", e.getMessage, "unknown config"))
        }
      case Right((simulationConfig, simHash, basicSimulation, json)) =>
        val (losses, wins) =
          SimulationRunner.run(
            basicSimulation,
            simulationConfig.simulationName,
            Math.min(10000, simulationConfig.simulations)
          )

        writeToDynamo(
          SimulationResult(
            simHash,
            simulationConfig.simulationName,
            s"wins: $wins, losses: $losses",
            json
          )
        )

        output.write(s"""{"wins":$wins,"losses":$losses}""".getBytes("UTF-8"))
    }
  }

  def parseSimulation(
      input: String
  ): Either[Error, (SimulationConfig, String, BasicSimulation, String)] =
    for {
      sqsMessage    <- decode[SQSMessage](input)
      message       = sqsMessage.Records.head
      configuration <- decode[SimulationConfig](message.body)
      simHash       = message.messageAttributes.simulationHash.stringValue
      parsedFocus   <- parseFocus(configuration.focus)
    } yield (
      configuration,
      simHash,
      BasicSimulation(configuration.players ++ configuration.monsters, parsedFocus),
      message.body
    )

  private def parseSimulationHash(
      input: String
  ): Either[Error, String] =
    for {
      sqsMessage <- decode[SQSMessage](input)
      message    = sqsMessage.Records.head
      simHash    = message.messageAttributes.simulationHash.stringValue
    } yield simHash

  private def writeToDynamo(simulationResult: SimulationResult): Unit = {
    val client = AmazonDynamoDBClientBuilder.standard().build()
    val table  = Table[SimulationResult]("simulation_results")

    Scanamo.exec(client) {
      table.put(simulationResult)
    } match {
      case Some(Left(dynamoError)) =>
        throw new RuntimeException(s"Error writing to DynamoDB ($dynamoError)")
      case _ => ()
    }
  }
}
