package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighterAbilities.actionSurge
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BaseFighterAbilitiesSpec extends UnitSpecBase {

  "Fighter" should {

    import Fighter._

    "utilise Two Weapon Fighting if equipped with two weapons" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

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
        val updatedFighter                          = updatedCreature.asInstanceOf[Fighter]

        updatedFighter.health shouldBe 4
        updatedFighter.abilityUsages.secondWindUsed shouldBe false
      }
    }

    "make 2 attacks using Extra Attack with a single Action" in {

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = fighter
            .withAllAbilitiesUsed()
            .withLevel(LevelFive)
            .withBaseWeapon(trackedMainSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(swordFighter, monster), LowestFirst)

          mainSwordUsedCount shouldBe 2
        }
      }
    }

    "make 2 attacks using Action Surge to make two Attack actions" in {

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = _abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelTwo)
            .withBaseWeapon(trackedMainSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(swordFighter, monster), LowestFirst)

          mainSwordUsedCount shouldBe 2
        }
      }
    }

    "make 4 attacks using Action Surge to make two Extra Attack actions" in {

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = _abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
            .withLevel(LevelFive)
            .withBaseWeapon(trackedMainSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(swordFighter, monster), LowestFirst)

          mainSwordUsedCount shouldBe 4
        }
      }
    }

    "use additional abilities in conjunction with Action Surge" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withLevel(LevelTwo)
            .withAbilities(List(actionSurge(1), trackedAbilityOne(2), trackedAbilityTwo(3)))
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(trackedAbilityFighter, monster), LowestFirst)

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

    def trackedAbilityOne(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
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

    def trackedAbilityTwo(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
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
