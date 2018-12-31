package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue

class BaseFighterAbilitiesSpec extends UnitSpecBase {

  "Fighter" should {

    "utilise Two Weapon Fighting if equipped with two weapons" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val dualWieldingFighter = fighter
          .copy(abilityUsages = fighter.abilityUsages.copy(secondWindUsed = true))
          .withWeapon(trackedSword)
          .withOffHand(trackedSword)
          .withFightingStyle(TwoWeaponFighting)
          .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(dualWieldingFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }

    "use Second Wind when it has reached a health threshold" in new TestContext {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val lowHealthFighter =
          fighter.withHealth(1).withMaxHealth(5).withLevel(LevelTwo).withAllAbilitiesUnused().withCombatIndex(1)

        val monster = testMonster.withCombatIndex(2)

        val Queue(_, Combatant(_, updatedFighter)) = Move.takeMove(Queue(lowHealthFighter, monster), LowestFirst)

        updatedFighter.health should (be > 1 and be <= 5)
      }
    }

    "use Action Surge to make another attack action" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordFighter =
          fighter
            .withLevel(LevelTwo)
            .copy(abilityUsages = fighter.abilityUsages.copy(secondWindUsed = true, actionSurgeUsed = false))
            .withWeapon(trackedSword)
            .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(swordFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
