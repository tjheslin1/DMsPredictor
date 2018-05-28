package simulation

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}

class BasicSimulationSpec extends FeatureSpec with Matchers {

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin") {
      val pcs  = List(Fighter())
      val mobs = List(Goblin())

      (1 to 100).map(_ => {
        val result = BasicSimulation(pcs, mobs).run

        result should (equal(SimulationResult(Win, "Fighter vs Goblin"))
          or equal(SimulationResult(Loss, "Fighter vs Goblin")))
      })
    }
  }
}
