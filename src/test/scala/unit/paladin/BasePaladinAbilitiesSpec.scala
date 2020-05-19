package unit.paladin

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladinAbilities._
import io.github.tjheslin1.dmspredictor.classes.paladin._
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class BasePaladinAbilitiesSpec extends UnitSpecBase {

  "Lay on Hands" should {
    "meet the condition if the Paladin has points left in its Lay on Hands pool" in {
      val paladin = random[Paladin].withLayOnHandsPoolOf(5).withCombatIndex(1)

      layOnHands(8)(paladin).conditionMet shouldBe true
    }

    "not meet the condition if the Paladin has no points left in its Lay on Hands pool" in {
      val noLayOnHandsPaladin = random[Paladin].withLayOnHandsPoolOf(0).withCombatIndex(1)

      layOnHands(1)(noLayOnHandsPaladin).conditionMet shouldBe false
    }

    "be triggered if an ally is below half their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(22).withMaxHealth(50).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe true
      }
    }

    "not be triggered if no allies are below half their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(10).withMaxHealth(10).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe false
      }
    }

    "not be triggered if no allies are alive" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(0).withMaxHealth(10).withIsAlive(false).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe false
      }
    }

    "updated the Paladin's Lay on Hands pool" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          val paladinCombatant = paladin.withLayOnHandsPoolOf(20).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(4).withMaxHealth(10).withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedPaladin.layOnHandsPool shouldBe 14
        }
      }
    }

    "heal the target for up to their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          val paladinCombatant = paladin.withLayOnHandsPoolOf(20).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(4).withMaxHealth(10).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 10
        }
      }
    }

    "heal the target using all remaining points in the Lay on Hands pool" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(10).withMaxHealth(30).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 15
        }
      }
    }

    "bring an unconscious ally back to consciousness" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val unconsciousHunter = hunter.withHealth(0).withMaxHealth(30).withIsAlive(true).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(unconsciousHunter), LowestFirst)

          updatedHunter.health shouldBe 5
        }
      }
    }

    "not bring a dead ally back to life" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(0).withMaxHealth(30).withIsAlive(false).withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 0
          updatedHunter.isAlive shouldBe false

          updatedPaladin.layOnHandsPool shouldBe 5
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
