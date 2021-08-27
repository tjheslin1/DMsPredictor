package unit.fighter

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.equipment.weapons.Greatsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BaseFighterAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Two Weapon Fighting" should {

    "be used if Player is equipped with two weapons" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingFighter = fighter
            .withFightingStyle(TwoWeaponFighting)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = twoWeaponFighting(Priority)(
            dualWieldingFighter).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
          offHAndSwordUsedCount shouldBe 1

          val mainHandDamage = 6 // 1 + 5 strength modifier
          val offHandDamage  = 6 // 1 + 5 strength modifier
          updatedMonster.health shouldBe monster.creature.health - (mainHandDamage + offHandDamage)
        }
      }
    }

    "not add the fighters stat modifier to the offhand attack if TwoWeaponFighting style is not chosen" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingFighter = fighter
            .withFightingStyle(Defense)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) = twoWeaponFighting(Priority)(
            dualWieldingFighter).useAbility(List(monster), LowestFirst)

          val mainHandDamage = 6 // 1 + 5 strength modifier
          val offHandDamage  = 1 // 1 + no strength modifier
          updatedMonster.health shouldBe monster.creature.health - (mainHandDamage + offHandDamage)
        }
      }
    }

    "set the player's bonus action to be used" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(19)

      val updatedFighter = twoWeaponFighting(Priority)(random[Fighter].withCombatIndex(1)).update
        .asInstanceOf[Fighter]

      updatedFighter.bonusActionUsed shouldBe true
    }

    "be used with Extra Attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingFighter = fighter
            .withAllAbilitiesUsed()
            .withFightingStyle(TwoWeaponFighting)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(1000).withCombatIndex(2)

          Move.takeMove(Queue(dualWieldingFighter, monster), LowestFirst)

          swordUsedCount shouldBe 2
          offHAndSwordUsedCount shouldBe 1
        }
      }
    }

    "meet the condition if the Fighter wields two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingFighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingFighter).conditionMet shouldBe true
    }

    "not meet the condition if the Fighter does not wield two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val fighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBaseWeapon(trackedSword)
        .withNoOffHand()
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(fighter).conditionMet shouldBe false
    }

    "not meet the condition if the Fighter wields a two-handed weapon" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val fighter = random[Fighter]
        .withFightingStyle(GreatWeaponFighting)
        .withBaseWeapon(Greatsword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(fighter).conditionMet shouldBe false
    }

    "not meet the condition if the Fighter has already used their bonus action this turn" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingFighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBonusActionUsed()
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingFighter).conditionMet shouldBe false
    }
  }

  "Second Wind" should {

    "be used when the below health condition has been met" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val lowHealthFighter = fighter
            .withLevel(LevelTwo)
            .withHealth(1)
            .withMaxHealth(5)
            .withCombatIndex(1)

          secondWind(Priority)(lowHealthFighter).triggerMet(List.empty[Combatant]) shouldBe true
        }
      }
    }

    "update usage when used" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val lowHealthFighter = fighter
            .withLevel(LevelTwo)
            .withHealth(1)
            .withMaxHealth(5)
            .withCombatIndex(1)

          val updatedFighter = secondWind(Priority)(lowHealthFighter).update.asInstanceOf[Fighter]

          updatedFighter.abilityUsages.secondWindUsed shouldBe true
        }
      }
    }

    "not be used when the below health condition has not been met" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val lowHealthFighter = fighter
            .withLevel(LevelTwo)
            .withHealth(4)
            .withMaxHealth(5)
            .withCombatIndex(1)

          secondWind(Priority)(lowHealthFighter).triggerMet(List.empty[Combatant]) shouldBe false
        }
      }
    }

    "updated bonus action used to true" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val updatedBaseFighter = secondWind(Priority)(fighter.withCombatIndex(1)).update
            .asInstanceOf[BaseFighter]

          updatedBaseFighter.bonusActionUsed shouldBe true
        }
      }
    }

    "updated second wind to used" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val freshFighter = fighter.withAllAbilitiesUnused().withCombatIndex(1)

          val updatedBaseFighter = secondWind(Priority)(freshFighter).update
            .asInstanceOf[BaseFighter]

          updatedBaseFighter.abilityUsages.secondWindUsed shouldBe true
        }
      }
    }
  }

  "Action Surge" should {

    "make two Attack actions" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = fighter
            .withLevel(LevelTwo)
            .withBaseWeapon(trackedSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(1000).withCombatIndex(2)

          actionSurge(Priority)(swordFighter).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 2
        }
      }
    }

    "make 4 attacks using two Extra Attack actions" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = Fighter._abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withHealth(1000).withCombatIndex(2)

          actionSurge(Priority)(swordFighter).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 4
        }
      }
    }

    "use additional abilities in conjunction with Action Surge" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withLevel(LevelTwo)
            .withAbilities(List(actionSurge(1), trackedAbility(2), otherTrackedAbility(3)))
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          actionSurge(Priority)(trackedAbilityFighter).useAbility(List(monster), LowestFirst)

          trackedAbilityUsedCount shouldBe 1
          otherTrackedAbilityUsedCount shouldBe 1
        }
      }
    }

    "updated the fighters action surge used to true" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val freshFighter = fighter.withAllAbilitiesUnused().withCombatIndex(1)

          val updatedFighter = actionSurge(Priority)(freshFighter).update.asInstanceOf[Fighter]

          updatedFighter.abilityUsages.actionSurgeUsed shouldBe true
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
