package unit

import io.github.tjheslin1.model.{Dice, InitiativeCalculator, Turn}
import org.scalatest.{Matchers, WordSpec}
import util.TestCreature

class TurnSpec extends WordSpec with Matchers {

  "run" should {
    "cycle through all creatures once" in {

      implicit val roll = Dice.defaultRandomiser

      val playerOne = TestCreature.player
      val playerTwo = TestCreature.player
      val enemy     = TestCreature.enemy

      val initiatives = InitiativeCalculator(List(playerOne, playerTwo, enemy)).rollInitiative

      Turn(initiatives).run.map(_.name) shouldBe initiatives.toSeq
        .map { case (_, initiative) => initiative }
        .sortBy(_.score)
        .reverse
        .map(_.creature.name)
    }
  }
}
