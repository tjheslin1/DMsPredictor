package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BaseFighterAbilitiesSpec extends UnitSpecBase {

  "Fighter" should {

    import Fighter._

    "utilise Two Weapon Fighting if equipped with two weapons" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val dualWieldingFighter = fighter.withAllAbilitiesUsed()
          .withFightingStyle(TwoWeaponFighting)
          .withBaseWeapon(trackedSword)
          .withOffHand(trackedSword)
          .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(dualWieldingFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }

    "use Second Wind when the below health condition has been met" in new TestContext {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val lowHealthFighter =
          fighter.withAllAbilitiesUnused().withHealth(1).withMaxHealth(5).withLevel(LevelTwo).withCombatIndex(1)

        val monster = testMonster.withCombatIndex(2)

        val Queue(_, Combatant(_, updatedFighter)) = Move.takeMove(Queue(lowHealthFighter, monster), LowestFirst)

        updatedFighter.health should (be > 1 and be <= 5)
      }
    }

    "not use Second Wind when the below health condition has not been met" in new TestContext {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val lowHealthFighter =
          fighter.withAllAbilitiesUnused().withHealth(4).withMaxHealth(5).withLevel(LevelTwo).withCombatIndex(1)

        val monster = testMonster.withCombatIndex(2)

        val Queue(_, Combatant(_, updatedCreature)) = Move.takeMove(Queue(lowHealthFighter, monster), LowestFirst)
        val updatedFighter = updatedCreature.asInstanceOf[Fighter]

        updatedFighter.health shouldBe 4
        updatedFighter.abilityUsages.secondWindUsed shouldBe false
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
            .withBaseWeapon(trackedSword)
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

        val swordFighter = _abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelTwo)
            .withBaseWeapon(trackedSword)
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

        val swordFighter = _abilityUsages.set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedSword)
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
