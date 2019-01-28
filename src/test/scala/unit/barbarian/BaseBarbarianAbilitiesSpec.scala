package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, BonusAction, WholeAction}
import util.TestData._

class BaseBarbarianAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Rage" should {

    "delegate to the next Action ability" in new TestContext {
      forAll { barbarian: Barbarian =>
        val trackedBarbarian = barbarian
          .withAbilities(List(rage(1), trackedBonusAction(2), trackedAbility(3)))
          .withCombatIndex(1)

        rage(Priority)(trackedBarbarian).update.asInstanceOf[BaseBarbarian]

        trackedAbilityUsedCount shouldBe 1
      }
    }

    "update the barbarian's number of rages left" in new TestContext {
      val barbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.rageUsages shouldBe 1
    }

    "update the barbarian's inRage to true" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val updatedBarbarian = rage(Priority)(ragedBarbarian).update.asInstanceOf[BaseBarbarian]

      updatedBarbarian.inRage shouldBe true
    }

    "reset the rage turns count back to 10" in new TestContext {
      val barbarian = random[Barbarian]
        .withRageUsagesLeft(2)
        .withRageTurnsLeft(5)
        .withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.rageTurnsLeft shouldBe 10
    }

    "add resistance to Bludgeoning, Piercing and Slashing damage" in new TestContext {
      val barbarian = random[Barbarian].withNoResistancesOrImmunities().withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.resistances shouldBe List(Bludgeoning, Piercing, Slashing)
    }

    "use the Barbarian's bonus action" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.bonusActionUsed shouldBe true
    }
  }

  "Reckless Attack" should {

    "set the Barbarian's attackStatus to Advantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val recklessBarbarian = recklessAttack(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      recklessBarbarian.attackStatus shouldBe Advantage
    }

    "set the Barbarian's defenseStatus to Disadvantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val recklessBarbarian = recklessAttack(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      recklessBarbarian.defenseStatus shouldBe Disadvantage
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false
    def trackedAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-one"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = WholeAction

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = trackedAbilityUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAbilityUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedAbilityUsed = true
          combatant.creature
        }
      }

    var trackedBonusActionUsedCount = 0
    var trackedBonusActionUsed      = false
    def trackedBonusAction(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-one"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = BonusAction

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = trackedBonusActionUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedBonusActionUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedBonusActionUsed = true
          combatant.creature
        }
      }
  }
}
