package simulation

import eu.timepit.refined.auto._
import io.github.tjheslin1.model.{BaseStats, Dice}
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}
import util.TestModel

class BasicSimulationSpec extends FeatureSpec with Matchers {

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin where Fighter wins") {
      val fighter = TestModel.player
      val goblin  = TestModel.enemy

      val creatures = List(fighter, goblin)

      val info = "Fighter vs Goblin"

      BasicSimulation(creatures).run(info)(Dice.naturalTwenty) shouldBe SimulationResult(Success, info)
    }

    scenario("One Fighter vs a Goblin where Goblin wins") {
      val fighter = TestModel.player
      val goblin  = TestModel.enemy.copy(stats = BaseStats(8, 16, 8, 8, 8, 8))

      val creatures = List(fighter, goblin)

      val info = "Fighter vs Goblin"

      BasicSimulation(creatures).run(info)(Dice.naturalTwenty) shouldBe SimulationResult(Loss, info)
    }
  }
}
