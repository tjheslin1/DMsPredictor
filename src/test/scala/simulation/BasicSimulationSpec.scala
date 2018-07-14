package simulation

import io.github.tjheslin1.model.{BaseStats, Dice}
import io.github.tjheslin1.simulation._
import org.scalatest.{FeatureSpec, Matchers}
import util.TestCreature

class BasicSimulationSpec extends FeatureSpec with Matchers {

  feature("BasicSimulation") {
    scenario("One Fighter vs a Goblin where Fighter wins") {
      val fighter = TestCreature.player
      val goblin  = TestCreature.enemy

      val creatures = List(fighter, goblin)

      val info = "Fighter vs Goblin"

      BasicSimulation(creatures).run(info)(Dice.naturalTwenty) shouldBe SimulationResult(Success, info)
    }

    scenario("One Fighter vs a Goblin where Goblin wins") {
      val fighter = TestCreature.player
      val goblin  = TestCreature.enemy.copy(stats = BaseStats(8, 16, 8, 8, 8, 8))

      val creatures = List(fighter, goblin)

      val info = "Fighter vs Goblin"

      BasicSimulation(creatures).run(info)(Dice.naturalTwenty) shouldBe SimulationResult(Loss, info)
    }
  }
}
