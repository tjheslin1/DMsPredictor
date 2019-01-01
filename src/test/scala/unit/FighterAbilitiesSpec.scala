package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import FighterAbilities._

import scala.collection.immutable.Queue

class FighterAbilitiesSpec extends UnitSpecBase {

  "Fighter" should {

    "utilise Two Weapon Fighting if equipped with two weapons" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val dualWieldingFighter = abilityUsagesLens
          .set(FighterAbilities(secondWindUsed = true, actionSurgeUsed = true))(fighter)
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

    "make 2 attacks using Extra Attack with a single Action" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordFighter = fighter.withAllAbilitiesUsed()
            .withLevel(LevelFive)
            .withWeapon(trackedSword)
            .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(swordFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }

    "make 2 attacks using Action Surge to make two Attack actions" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordFighter = abilityUsagesLens
            .set(FighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelTwo)
            .withWeapon(trackedSword)
            .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(swordFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }

    "make 4 attacks using Action Surge to make two Extra Attack actions" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordFighter =
          fighter
            .withLevel(LevelFive)
            .copy(abilityUsages = fighter.abilityUsages.copy(secondWindUsed = true, actionSurgeUsed = false))
            .withWeapon(trackedSword)
            .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(swordFighter, monster), LowestFirst)

        swordUsedCount shouldBe 4
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
