package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class CoreAbilitiesSpec extends UnitSpecBase {

  "Extra Attack" should {
    "delegate to an ability lower in the order which can be used during an Attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withAllAbilitiesUsed()
            .withAbilities(List(extraAttack(1), trackedActionAbility(2), trackedAttackAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          Move.takeMove(Queue(trackedAbilityFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

          trackedAttackUsedCount shouldBe 2
          trackedActionAbilityUsedCount shouldBe 0
        }
      }
    }

    "delegate to an ability lower in the order then default to an attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withAllAbilitiesUsed()
            .withAbilities(List(extraAttack(1), trackedActionAbility(2), singleUseAttackAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          Move.takeMove(Queue(trackedAbilityFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

          trackedAttackUsedCount shouldBe 1
          trackedActionAbilityUsedCount shouldBe 0
        }
      }
    }

    "use up a Player's Bonus Action" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val extraAttackFighter = fighter
            .withAllAbilitiesUsed()
            .withAbilities(List(extraAttack(1)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = Move.takeMove(Queue(extraAttackFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

          updatedFighter.bonusActionUsed shouldBe true
        }
      }
    }

    "make two weapon attacks where no other abilities are available" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordedFighter = fighter
          .withAllAbilitiesUsed()
          .withBaseWeapon(trackedSword)
          .withAbilities(List(extraAttack(1)))
          .withLevel(LevelFive)
          .withCombatIndex(1)

        Move.takeMove(Queue(swordedFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var trackedAttackUsedCount = 0

    def trackedAttackAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-single-attack"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = SingleAttack

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = true

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, target)
        }

      def update: Creature = {
        combatant.creature
      }
    }

    var singleUseAttackAbilityUsed = false

    def singleUseAttackAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-single-use"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = SingleAttack

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = singleUseAttackAbilityUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, target)
        }

      def update: Creature = {
        singleUseAttackAbilityUsed = true
        combatant.creature
      }
    }

    var trackedActionAbilityUsedCount = 0
    var trackedActionAbilityUsed      = false

    def trackedActionAbility(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String     = "test-tracked-ability-action"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      val triggerMet: Boolean   = true
      def conditionMet: Boolean = trackedActionAbilityUsed == false

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
        trackedActionAbilityUsedCount += 1
        (combatant, target)
      }

      def update: Creature = {
        trackedActionAbilityUsed = true
        combatant.creature
      }
    }
  }
}
