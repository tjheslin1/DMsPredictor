package unit

import io.github.tjheslin1.model.{InitiativeCalculator, Turn}
import org.scalatest.{Matchers, WordSpec}
import util.TestCreature

class TurnSpec extends WordSpec with Matchers {

  "run" should {
    "cycle through all creatures once" in {

      import io.github.tjheslin1.model.Dice._

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
