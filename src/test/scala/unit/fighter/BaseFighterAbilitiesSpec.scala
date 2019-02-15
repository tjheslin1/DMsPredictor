package unit.fighter

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
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
            .withBaseWeapon(trackedMainSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          twoWeaponFighting(Priority)(dualWieldingFighter).useAbility(monster.some)

          mainSwordUsedCount shouldBe 1
          offHAndSwordUsedCount shouldBe 1
        }
      }
    }

    "set the player's bonus action to be used" in {
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val updatedFighter =
            twoWeaponFighting(Priority)(random[Fighter].withCombatIndex(1)).update.asInstanceOf[Fighter]

          updatedFighter.bonusActionUsed shouldBe true
        }
    }

    "be used with Extra Attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingFighter = fighter
            .withAllAbilitiesUsed()
            .withFightingStyle(TwoWeaponFighting)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedMainSword)
            .withOffHand(trackedOffHandSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(dualWieldingFighter, monster), LowestFirst)

          mainSwordUsedCount shouldBe 2
          offHAndSwordUsedCount shouldBe 1
        }
      }
    }

    "meet the condition if the Player wields two weapons" in new TestContext {
      val dualWieldingFighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBaseWeapon(trackedMainSword)
        .withOffHand(trackedOffHandSword)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingFighter).conditionMet shouldBe true
    }

    "not meet the condition if the Player does not wield two weapons" in new TestContext {
      val fighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBaseWeapon(trackedMainSword)
        .withNoOffHand()
        .withLevel(LevelFive)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(fighter).conditionMet shouldBe false
    }

    "not meet the condition if the Player does not have the Two Weapon Fighting fighting style" in new TestContext {
      val fighter = random[Fighter]
        .withFightingStyle(GreatWeaponFighting)
        .withBaseWeapon(trackedMainSword)
        .withNoOffHand()
        .withLevel(LevelFive)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(fighter).conditionMet shouldBe false
    }

    "not meet the condition if the Player has already used their bonus action this turn" in new TestContext {
      val dualWieldingFighter = random[Fighter]
        .withFightingStyle(TwoWeaponFighting)
        .withBonusActionUsed()
        .withBaseWeapon(trackedMainSword)
        .withOffHand(trackedOffHandSword)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingFighter).conditionMet shouldBe false
    }
  }

  "Second Wind" should {

    "be used when the below health condition has been met" in new TestContext {
      forAll { fighter: Fighter =>
        val lowHealthFighter =
          fighter.withHealth(1).withMaxHealth(5).withLevel(LevelTwo).withCombatIndex(1)

        secondWind(Priority)(lowHealthFighter).triggerMet shouldBe true
      }
    }

    "update usage when used" in new TestContext {
      forAll { fighter: Fighter =>
        val lowHealthFighter =
          fighter.withHealth(1).withMaxHealth(5).withLevel(LevelTwo).withCombatIndex(1)

        val updatedFighter = secondWind(Priority)(lowHealthFighter).update.asInstanceOf[Fighter]

        updatedFighter.abilityUsages.secondWindUsed shouldBe true
      }
    }

    "not be used when the below health condition has not been met" in new TestContext {
      forAll { fighter: Fighter =>
        val lowHealthFighter =
          fighter
            .withHealth(4)
            .withMaxHealth(5)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

        secondWind(Priority)(lowHealthFighter).triggerMet shouldBe false
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
            .withBaseWeapon(trackedMainSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          actionSurge(Priority)(swordFighter).useAbility(monster.some)

          mainSwordUsedCount shouldBe 2
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
            .withBaseWeapon(trackedMainSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          actionSurge(Priority)(swordFighter).useAbility(monster.some)

          mainSwordUsedCount shouldBe 4
        }
      }
    }

    "use additional abilities in conjunction with Action Surge" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withLevel(LevelTwo)
            .withAbilities(List(actionSurge(1), trackedAbilityOne(2), trackedAbilityTwo(3)))
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          actionSurge(Priority)(trackedAbilityFighter).useAbility(monster.some)

          trackedAbilityOneUsedCount shouldBe 1
          trackedAbilityTwoUsedCount shouldBe 1
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var mainSwordUsedCount = 0
    val trackedMainSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      mainSwordUsedCount += 1
      1
    })

    var offHAndSwordUsedCount = 0
    val trackedOffHandSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      offHAndSwordUsedCount += 1
      1
    })

    var trackedAbilityOneUsedCount = 0
    var trackedAbilityOneUsed      = false

    def trackedAbilityOne(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-one"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = WholeAction

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = trackedAbilityOneUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAbilityOneUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedAbilityOneUsed = true
          combatant.creature
        }
      }

    var trackedAbilityTwoUsedCount = 0
    var trackedAbilityTwoUsed      = false

    def trackedAbilityTwo(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-two"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = WholeAction

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = trackedAbilityTwoUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAbilityTwoUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedAbilityTwoUsed = true
          combatant.creature
        }
      }
  }
}
