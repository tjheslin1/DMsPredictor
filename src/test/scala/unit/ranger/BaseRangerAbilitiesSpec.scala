package unit.ranger

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities._
import io.github.tjheslin1.dmspredictor.classes.ranger._
import io.github.tjheslin1.dmspredictor.equipment.armour.Shield
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BaseRangerAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Two Weapon Fighting" should {
    "be used if Player is equipped with two weapons" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingRanger = ranger
            .withFightingStyle(TwoWeaponFighting)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            twoWeaponFighting(Priority)(dualWieldingRanger).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
          offHAndSwordUsedCount shouldBe 1

          val mainHandDamage = 6 // 1 + 5 strength modifier
          val offHandDamage  = 6 // 1 + 5 strength modifier
          updatedMonster.health shouldBe monster.creature.health - (mainHandDamage + offHandDamage)
        }
      }
    }

    "not add the rangers stat modifier to the offhand attack if TwoWeaponFighting style is not chosen" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingRanger = ranger
            .withFightingStyle(Defense)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            twoWeaponFighting(Priority)(dualWieldingRanger).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
          offHAndSwordUsedCount shouldBe 1

          val mainHandDamage = 6 // 1 + 5 strength modifier
          val offHandDamage  = 1 // 1 + no strength modifier
          updatedMonster.health shouldBe monster.creature.health - (mainHandDamage + offHandDamage)
        }
      }
    }

    "set the player's bonus action to be used" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(19)

      val updatedRanger =
        twoWeaponFighting(Priority)(random[Ranger].withCombatIndex(1)).update
          .asInstanceOf[Ranger]

      updatedRanger.bonusActionUsed shouldBe true
    }

    "be used with Extra Attack" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingRanger = ranger
            .withFightingStyle(TwoWeaponFighting)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(1000).withCombatIndex(2)

          Move.takeMove(Queue(dualWieldingRanger, monster), LowestFirst)

          swordUsedCount shouldBe 2
          offHAndSwordUsedCount shouldBe 1
        }
      }
    }

    "meet the condition if the Ranger wields two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRanger = random[Ranger]
        .withFightingStyle(TwoWeaponFighting)
        .withLevel(LevelTwo)
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRanger).conditionMet shouldBe true
    }

    "not meet the condition if the Ranger does not wield two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRanger = random[Ranger]
        .withFightingStyle(TwoWeaponFighting)
        .withLevel(LevelTwo)
        .withBaseWeapon(trackedSword)
        .withOffHand(Shield)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRanger).conditionMet shouldBe false
    }

    "not meet the condition if the Ranger is not at least level two" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRanger = random[Ranger]
        .withFightingStyle(TwoWeaponFighting)
        .withLevel(LevelOne)
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRanger).conditionMet shouldBe false
    }

    "not meet the condition if the Fighter wields a two-handed weapon" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRanger = random[Ranger]
        .withFightingStyle(Defense)
        .withLevel(LevelTwo)
        .withBaseWeapon(Greatsword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRanger).conditionMet shouldBe false
    }

    "not meet the condition if the Ranger has already used their bonus action this turn" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRanger = random[Ranger]
        .withFightingStyle(TwoWeaponFighting)
        .withBonusActionUsed()
        .withLevel(LevelTwo)
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRanger).conditionMet shouldBe false
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
