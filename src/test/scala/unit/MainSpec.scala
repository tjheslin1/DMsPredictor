package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.Main
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger.rangerSpellSlots
import io.github.tjheslin1.dmspredictor.classes.ranger.{Archery, BaseRanger, Hunter}
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.equipment.armour.{ChainShirt, NoArmour}
import io.github.tjheslin1.dmspredictor.equipment.weapons.{Longbow, Shortsword}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import org.scalatest.EitherValues

class MainSpec extends UnitSpecBase with EitherValues {

  "parseSimulation" should {

    "parse SQS message for wizard" in {

      val sqsMessage =
        """
          |{
          |    "Records": [
          |        {
          |            "messageId": "c9a6dca2-ccc2-41ba-92ca-588177fd5f5d",
          |            "receiptHandle": "AQEBDNzhoPWsq/oMuBG7BnrzGbe8tYYH47M14hgZUTWlX0zNWdg+KXQYDecHv+RKKvD3lbPRtpiUv6ahS26Z7JXe2FI4y93XZp6l5wr9GLxYVInhS6gTtfjC1AivqzRGcKavUMT9yeUYMuC2B1iHG7SQTLuhF5B1i3gVftsELIGZ/9Mmp+UpfJllYI4rfdT0TARDK1/0e7RwGlyHlq6RfXPxlgay+hlhJlLVgGJpDbk+XhAV+WfnA1MF2pENFA/T1GrEuuu/x8KGOePfIURu4n/k0TIVWqkNTv1lILQOvHNcBOkN1pMy9DjyEkZhgge2lxt18i2wDBtPn/T336pR19BTILmBnvxLyxXE4LqdDbULOBzkGlSsGQLZ4Z5OuL4gz2wqq36lIBY3BRvhY6AhqIufBg==",
          |            "body": "{\"simulationName\":\"Wizard vs Goblin\",\"simulations\":100,\"focus\":\"LowestFirst\",\"players\":[{\"class\":\"wizard\",\"level\":4,\"stats\":\"10,10,14,14,14,10\",\"weapon\":\"Shortsword\",\"armour\":\"NoArmour\",\"offHand\":\"none\",\"skills\":\"1,1\",\"name\":\"TestWizard\"}],\"monsters\":[{\"monster\":\"goblin\",\"name\":\"TestGoblin\"}]}",
          |            "attributes": {
          |                "ApproximateReceiveCount": "1",
          |                "SentTimestamp": "1572678765238",
          |                "SenderId": "AROATJ7IRC6CX6JDFQPOB:queue_submit_function",
          |                "ApproximateFirstReceiveTimestamp": "1572678874547"
          |            },
          |            "messageAttributes": {
          |                "simulationHash": {
          |                    "stringValue": "1512201669",
          |                    "stringListValues": [],
          |                    "binaryListValues": [],
          |                    "dataType": "String"
          |                }
          |            },
          |            "md5OfMessageAttributes": "771d5a26f2f7d6d9d62515e8b4d763db",
          |            "md5OfBody": "9521c8eeabfa603f39fd642b4cbf61aa",
          |            "eventSource": "aws:sqs",
          |            "eventSourceARN": "arn:aws:sqs:eu-west-2:67876567:simulation-queue",
          |            "awsRegion": "eu-west-2"
          |        }
          |    ]
          |}
        """.stripMargin

      val expectedPlayers = List(
        Wizard(
          LevelFour,
          26,
          26,
          BaseStats(10, 10, 14, 14, 14, 10),
          Shortsword,
          Skills(1, 1),
          Wizard.wizardSpellSlots(LevelFour),
          Wizard.standardWizardSpellList,
          armour = NoArmour,
          proficiencyBonus = ProficiencyBonus.fromLevel(LevelFour),
          name = "TestWizard"
        ))

//      val expectedMonsters = List(Goblin(6, 6, name = "TestGoblin"))

      val (_, simHash, basicSim, _) = new Main().parseSimulation(sqsMessage) match {
        case Left(error) =>
          println(s"Error: ${error.getMessage}")
          throw error
        case Right(result) => result
      }

//      simConfig shouldBe SimulationConfig(
//        "Wizard vs Goblin",
//        100,
//        "LowestFirst",
//        expectedPlayers,
//        expectedMonsters
//      )

      simHash shouldBe "1512201669"

      basicSim.creatures.filter(_.creatureType == PlayerCharacter) shouldBe expectedPlayers
      basicSim.focus shouldBe LowestFirst
    }

    "parse SQS message for ranger" in {

      val sqsMessage =
        """
          |{
          |    "Records": [
          |        {
          |            "messageId": "c9a6dca2-ccc2-41ba-92ca-588177fd5f5d",
          |            "receiptHandle": "AQEBDNzhoPWsq/oMuBG7BnrzGbe8tYYH47M14hgZUTWlX0zNWdg+KXQYDecHv+RKKvD3lbPRtpiUv6ahS26Z7JXe2FI4y93XZp6l5wr9GLxYVInhS6gTtfjC1AivqzRGcKavUMT9yeUYMuC2B1iHG7SQTLuhF5B1i3gVftsELIGZ/9Mmp+UpfJllYI4rfdT0TARDK1/0e7RwGlyHlq6RfXPxlgay+hlhJlLVgGJpDbk+XhAV+WfnA1MF2pENFA/T1GrEuuu/x8KGOePfIURu4n/k0TIVWqkNTv1lILQOvHNcBOkN1pMy9DjyEkZhgge2lxt18i2wDBtPn/T336pR19BTILmBnvxLyxXE4LqdDbULOBzkGlSsGQLZ4Z5OuL4gz2wqq36lIBY3BRvhY6AhqIufBg==",
          |            "body": "{\"simulationName\":\"rangervgoblin\",\"simulations\":\"5\",\"focus\":\"LowestFirst\",\"players\":[{\"class\":\"ranger\",\"name\":\"TestRanger\",\"level\":\"5\",\"weapon\":\"longbow\",\"armour\":\"chainshirt\",\"offHand\":\"none\",\"stats\":\"14,14,14,14,14,14\",\"skills\":\"1,1 \",\"rangerFightingStyle\":\"archery\"}],\"monsters\":[{\"name\":\"gggg\",\"monster\":\"goblin\"}]}",
          |            "attributes": {
          |                "ApproximateReceiveCount": "1",
          |                "SentTimestamp": "1572678765238",
          |                "SenderId": "AROATJ7IRC6CX6JDFQPOB:queue_submit_function",
          |                "ApproximateFirstReceiveTimestamp": "1572678874547"
          |            },
          |            "messageAttributes": {
          |                "simulationHash": {
          |                    "stringValue": "1512201769",
          |                    "stringListValues": [],
          |                    "binaryListValues": [],
          |                    "dataType": "String"
          |                }
          |            },
          |            "md5OfMessageAttributes": "771d5a26f2f7d6d9d62515e8b4d763db",
          |            "md5OfBody": "9521c8eeabfa603f39fd642b4cbf61aa",
          |            "eventSource": "aws:sqs",
          |            "eventSourceARN": "arn:aws:sqs:eu-west-2:67876567:simulation-queue",
          |            "awsRegion": "eu-west-2"
          |        }
          |    ]
          |}
        """.stripMargin

      val expectedPlayers = List(
        Hunter(
          LevelFive,
          44,
          44,
          BaseStats(14, 14, 14, 14, 14, 14),
          Longbow,
          Skills(1, 1),
          rangerSpellSlots(LevelFive),
          armour = ChainShirt,
          fightingStyles = List(Archery),
          proficiencyBonus = ProficiencyBonus.fromLevel(LevelFive),
          name = "TestRanger"
        ))

      val (_, simHash, basicSim, _) = new Main().parseSimulation(sqsMessage) match {
        case Left(error) =>
          println(s"Error: ${error.getMessage}")
          throw error
        case Right(result) => result
      }

      simHash shouldBe "1512201769"

      basicSim.creatures.filter(_.creatureType == PlayerCharacter) shouldBe expectedPlayers
      basicSim.focus shouldBe LowestFirst
    }
  }
}
