package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model.{BaseStats, LevelFour, ProficiencyBonus, Skills}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.simulation.BasicSimulation
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.{Main, SimulationConfig}
import org.scalatest.EitherValues

class MainSpec extends UnitSpecBase with EitherValues {

  "parseSimulation" should {
    "parse SQS message" in {

      val sqsMessage =
        """
          |{
          |    "Records": [
          |        {
          |            "messageId": "9588f02e-89cc-42fb-bdb9-76a7482d4371",
          |            "receiptHandle": "AQEBgNEyat7bN4fdU99P3Ds8YHCxSlo4HW0/LX+5jLsDaHinzO7fSQO9p5QdVrk4/xFqtBSm0hMnTu1nWk9ZoW+4xytMQiMh9FVeYjpxuvl3XFHlGzaPCm7hs53MwaIv46g8O/GRPJ3UgBA77jGO8RI69tmFi73FnSOv3a0X8/n6lbDFXqcO3qDFV3SOUpJRoVNeipFpJW49JAxRPB8wLTEOZ+od13Uesv7gipbkp0uyyDaSJNspTQ9LpJjMXmdm5iLxXxw2uc325Fu3cisRMYRoy2f2Q43rMwRHwKamM+lG6qgZ6l3L39cyIQ9Wr7GkCtH6+gdQs6cJD+y1UoRM/e4kNFnlPfrmkOeWCwKKXK4pF4Y6+gzVBOtOMLLGHpoNk61bHdjDympgIskTwsXJLgBNLA==",
          |            "body": "{\"simulationName\":\"Wizard vs Goblin\",\"simulations\":100,\"focus\":\"LowestFirst\",\"players\":[{\"class\":\"wizard\",\"level\":4,\"stats\":\"10,10,14,14,14,10\",\"weapon\":\"Shortsword\",\"skills\":\"1,1\",\"name\":\"TestWizard\"}],\"monsters\":[{\"monster\":\"goblin\",\"name\":\"TestGoblin\"}]}",
          |            "attributes": {
          |                "ApproximateReceiveCount": "1",
          |                "SentTimestamp": "1571686504055",
          |                "SenderId": "AROATJ7IRC6CX6JDFQPOB:queue_submit_function",
          |                "ApproximateFirstReceiveTimestamp": "1571686504059"
          |            },
          |            "messageAttributes": {
          |                "simulationHash": {
          |                    "stringValue": "1377404223",
          |                    "stringListValues": [],
          |                    "binaryListValues": [],
          |                    "dataType": "String"
          |                }
          |            },
          |            "md5OfMessageAttributes": "2cab3a725abdb6f4c776c6c4c611ac45",
          |            "md5OfBody": "9521c8eeabfa603f39fd642b4cbf61aa",
          |            "eventSource": "aws:sqs",
          |            "eventSourceARN": "arn:aws:sqs:eu-west-2:227584055173:simulation-queue",
          |            "awsRegion": "eu-west-2"
          |        }
          |    ]
          |}
        """.stripMargin

      val expectedPlayers = List(
        Wizard(
          LevelFour,
          12,
          12,
          BaseStats(10, 10, 14, 14, 14, 10),
          Shortsword,
          Skills(1, 1),
          Wizard.wizardSpellSlots(LevelFour),
          Wizard.standardWizardSpellList,
          proficiencyBonus = ProficiencyBonus.fromLevel(LevelFour),
          name = "TestWizard"
        ))

      val expectedMonsters = List(Goblin(1, 1, name = "TestGoblin"))

      val (simConfig, simHash, basicSim) = new Main().parseSimulation(sqsMessage) match {
        case Left(error) => println(s"Error: ${error.getMessage}"); throw error
        case Right(result) => result
      }

      simConfig shouldBe SimulationConfig(
        "Wizard vs Goblin",
        100,
        "LowestFirst",
        expectedPlayers,
        expectedMonsters
      )

      simHash shouldBe "1377404223"

      basicSim shouldBe BasicSimulation(expectedPlayers ++ expectedMonsters, LowestFirst)
    }
  }
}
