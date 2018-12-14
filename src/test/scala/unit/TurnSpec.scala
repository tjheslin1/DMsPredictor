package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class TurnSpec extends UnitSpecBase {

  "run" should {
    "cycle through all creatures once" in {
      forAll { (fighterOne: Fighter, fighterTwo: Fighter, monster: TestMonster) =>
        implicit val roll = Dice.defaultRandomiser

        val initiatives =
          InitiativeCalculator(List(fighterOne.creature, fighterTwo.creature, monster.testMonster)).rollInitiative()

        Turn(initiatives).run(LowestFirst).map(_.creature.name) shouldBe initiatives.toSeq
          .map { case (_, initiative) => initiative }
          .sortBy(_.score)
          .reverse
          .map(_.combatant.creature.name)
      }
    }
  }
}
