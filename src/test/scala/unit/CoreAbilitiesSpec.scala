package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.{Ability, LowestFirst}
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class CoreAbilitiesSpec extends UnitSpecBase {

  "Extra Attack" should {
    "delegate to a lower priority ability which can be used during an Attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter.withAllAbilitiesUsed()
            .withAbilities(List(1 -> CoreAbilities.extraAttack, 2 -> trackedManeuverAbility))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          Move.takeMove(Queue(trackedAbilityFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

          trackedAbilityUsedCount shouldBe 1
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

        val swordedFighter = fighter.withAllAbilitiesUsed()
          .withBaseWeapon(trackedSword)
          .withAbilities(List(CoreAbilities.extraAttack(1)))
          .withLevel(LevelFive)
          .withCombatIndex(1)

        Move.takeMove(Queue(swordedFighter, testMonster.withArmourClass(5).withCombatIndex(2)), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false

    def trackedManeuverAbility(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String = "test-tracked-ability-one"

      val levelRequirement: Level = LevelOne
      val triggerMet: Boolean     = true
      val conditionMet: Boolean   = trackedAbilityUsed == false

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
