package simulation

import base.PropertyChecksBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.D20
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.simulation._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util._
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should.Matchers

class BasicSimulationSpec extends AnyFeatureSpec with Matchers with PropertyChecksBase with TestData {

  Feature("BasicSimulation") {

    val info = "Fighter vs TestMonster"

    Scenario("One Fighter vs a TestMonster where Fighter wins") {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val healthyFighter = fighter.withHealth(1000).withStrength(10).withDexterity(10)
        val weakTestMonster = goblin.withHealth(1)

        BasicSimulation(List(healthyFighter, weakTestMonster), LowestFirst)
          .run(info)(D20.naturalTwenty) shouldBe SimulationResult(Success, info)
      }
    }

    Scenario("One Fighter vs a TestMonster where TestMonster wins") {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val weakFighter = fighter.withHealth(1)
        val healthyTestMonster = goblin.withHealth(1000).withStrength(10)

        BasicSimulation(List(weakFighter, healthyTestMonster), LowestFirst)
          .run(info)(D20.naturalTwenty) shouldBe SimulationResult(Loss, info)
      }
    }
  }
}
