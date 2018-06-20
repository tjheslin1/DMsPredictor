package simulation

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}

class BasicSimulationSpec extends FeatureSpec with Matchers {

  import io.github.tjheslin1.model.Dice._

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin") {
      val pcs  = List(Fighter())
      val mobs = List(Goblin())

      BasicSimulation(pcs, mobs).run should (equal(SimulationResult(Success, "Fighter vs Goblin"))
        or equal(SimulationResult(Loss, "Fighter vs Goblin")))
    }
  }
}
