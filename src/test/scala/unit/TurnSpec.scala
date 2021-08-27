package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst

class TurnSpec extends UnitSpecBase {

  "run" should {
    "cycle through all creatures once" in {
      forAll { (fighterOne: Fighter, fighterTwo: Fighter, goblin: Goblin) =>
        implicit val roll: RollStrategy = Dice.defaultRandomiser

        val initiatives = InitiativeCalculator(List(fighterOne, fighterTwo, goblin))
          .rollInitiative()

        Turn(initiatives).run(LowestFirst).map(_.creature.name) shouldBe initiatives.toSeq
          .map { case (_, initiative) =>
            initiative
          }
          .sortBy(_.score)
          .reverse
          .map(_.combatant.creature.name)
      }
    }
  }
}
