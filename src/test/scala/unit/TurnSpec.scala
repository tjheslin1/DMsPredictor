package unit

import base.PropertyChecksBase
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.{Dice, InitiativeCalculator, Turn}
import io.github.tjheslin1.strategy.LowestFirst
import org.scalatest.{Matchers, WordSpec}

class TurnSpec extends WordSpec with Matchers with PropertyChecksBase {

  "run" should {
    "cycle through all creatures once" in {
      forAll { (fighterOne: Fighter, fighterTwo: Fighter, monster: TestMonster) =>
        implicit val roll = Dice.defaultRandomiser

        val initiatives = InitiativeCalculator(List(fighterOne.creature, fighterTwo.creature, monster.creature)).rollInitiative

        Turn(initiatives).run(LowestFirst).map(_.name) shouldBe initiatives.toSeq
          .map { case (_, initiative) => initiative }
          .sortBy(_.score)
          .reverse
          .map(_.creature.name)
      }
    }
  }
}
