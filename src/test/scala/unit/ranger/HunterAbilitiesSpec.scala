package unit.ranger

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.classes.ranger.HunterAbilities._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class HunterAbilitiesSpec extends UnitSpecBase {

  "Colossus Slayer" should {
    "deal damage if the enemy is below max health" in {
      forAll { (hunter: Hunter, goblin: Goblin) =>
        implicit val roll: RollStrategy = _ => RollResult(10)

        val hunterCombatant = hunter.withCombatIndex(1)

        val goblinCombatant = goblin
          .withHealth(45)
          .withMaxHealth(50)
          .withCombatIndex(2)

        colossusSlayer(1)(hunterCombatant).triggerMet(List(goblinCombatant)) shouldBe true
      }
    }

    "not deal damage if the enemy is at max health" in {
      forAll { (hunter: Hunter, goblin: Goblin) =>
        implicit val roll: RollStrategy = _ => RollResult(10)

        val hunterCombatant = hunter.withCombatIndex(1)

        val goblinCombatant = goblin
          .withHealth(50)
          .withMaxHealth(50)
          .withCombatIndex(2)

        colossusSlayer(1)(hunterCombatant).triggerMet(List(goblinCombatant)) shouldBe false
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
