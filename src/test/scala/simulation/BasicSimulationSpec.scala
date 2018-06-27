package simulation

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}

class BasicSimulationSpec extends FeatureSpec with Matchers {

  import io.github.tjheslin1.model.Dice._

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin") {

      val creatures = List(Fighter.levelOneFighter().creature, Goblin.levelOneGoblin().creature)

      val info = "Fighter vs Goblin"

      BasicSimulation(creatures).run(info) should (equal(SimulationResult(Success, info))
        or equal(SimulationResult(Loss, info)))
    }
  }
}
