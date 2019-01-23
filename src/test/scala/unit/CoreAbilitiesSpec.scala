package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability.{Action, SingleAttack}
import io.github.tjheslin1.dmspredictor.strategy.{Ability, LowestFirst}
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
            .withAbilities(List(extraAttack(1), trackedAbility(2), trackedManeuverAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          Move.takeMove(Queue(trackedAbilityFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

          fail("Need to utilise AbilityAction")
          
          trackedManeuverUsedCount shouldBe 1
          trackedAbilityUsedCount shouldBe 0
        }
      }
    }

    "make two weapon attacks" in new TestContext {
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

    var trackedManeuverUsedCount = 0
    var trackedManeuverUsed      = false

    def trackedManeuverAbility(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String     = "test-tracked-manveuver-one"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = SingleAttack

      val triggerMet: Boolean   = true
      val conditionMet: Boolean = trackedManeuverUsed == false

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
        trackedManeuverUsedCount += 1
        (combatant, target)
      }

      def update: Creature = {
        trackedManeuverUsed = true
        combatant.creature
      }
    }

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false

    def trackedAbility(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String     = "test-tracked-ability-one"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = Action

      val triggerMet: Boolean   = true
      val conditionMet: Boolean = trackedAbilityUsed == false

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
        trackedAbilityUsedCount += 1
        (combatant, target)
      }

      def update: Creature = {
        trackedAbilityUsed = true
        combatant.creature
      }
    }
  }
}
