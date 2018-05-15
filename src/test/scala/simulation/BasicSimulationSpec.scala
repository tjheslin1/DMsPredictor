package simulation

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}

class BasicSimulationSpec extends FeatureSpec with Matchers {

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin") {
      BasicSimulation(List(Fighter()), List(Goblin())).run should (equal(SimulationResult(Win, "Fighter vs Goblin"))
        or equal(SimulationResult(Loss, "Fighter vs Goblin")))
    }
  }
}
