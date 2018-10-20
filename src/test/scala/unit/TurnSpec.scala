package unit

import base.PropertyChecksBase
import io.github.tjheslin1.model.{Creature, Dice, InitiativeCalculator, Monster, PlayerCharacter, Turn}
import org.scalatest.{Matchers, WordSpec}

class TurnSpec extends WordSpec with Matchers with PropertyChecksBase {

  "run" should {
    "cycle through all creatures once" in {
      forAll { (c1: Creature, c2: Creature, c3: Creature) =>
        implicit val roll = Dice.defaultRandomiser

        val playerOne = c1.copy(creatureType = PlayerCharacter)
        val playerTwo = c2.copy(creatureType = PlayerCharacter)
        val enemy     = c3.copy(creatureType = Monster)

        val initiatives = InitiativeCalculator(List(playerOne, playerTwo, enemy)).rollInitiative

        Turn(initiatives).run.map(_.name) shouldBe initiatives.toSeq
          .map { case (_, initiative) => initiative }
          .sortBy(_.score)
          .reverse
          .map(_.creature.name)
      }
    }
  }
}
