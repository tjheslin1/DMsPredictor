package unit

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.{InitiativeCalculator, Turn}
import io.github.tjheslin1.monsters.Goblin
import org.scalatest.{Matchers, WordSpec}

class TurnSpec extends WordSpec with Matchers {

  "run" should {
    "cycle through all creatures once" in {

      import io.github.tjheslin1.model.Dice._

      val fighterOne = Fighter.levelOneFighter().creature
      val fighterTwo = Fighter.levelOneFighter().creature
      val goblin = Goblin.levelOneGoblin().creature

      val initiatives = InitiativeCalculator(List(fighterOne, fighterTwo, goblin)).rollInitiative

      Turn(initiatives).run.map(_.name) shouldBe initiatives.toSeq.sortBy(_._2.value).reverse.map(_._2.creature.name)
    }
  }
}
