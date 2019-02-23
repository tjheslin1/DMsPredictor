package unit.barbarian

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, BonusAction, WholeAction}
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import util.TestData._
import util.TestMonster

class BaseBarbarianAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Rage" should {

    "delegate to the next Action ability" in new TestContext {
      forAll { (barbarian: Barbarian, testMonster: TestMonster) =>
        val trackedBarbarian = barbarian
          .withAbilities(List(rage(1), trackedBonusAction(2), trackedAbility(3)))
          .withCombatIndex(1)

        val monster = testMonster.withCombatIndex(2)

        rage(Priority)(trackedBarbarian).useAbility(List(monster), LowestFirst)

        trackedAbilityUsedCount shouldBe 1
      }
    }

    "update the barbarian's number of rages left" in new TestContext {
      val barbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, ragingBarbarian: Barbarian), _) =
        rage(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      ragingBarbarian.rageUsages shouldBe 1
    }

    "update the barbarian's inRage to true" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, ragingBarbarian: Barbarian), _) =
        rage(Priority)(ragedBarbarian).useAbility(List.empty[Combatant], LowestFirst)

      ragingBarbarian.inRage shouldBe true
    }

    "set the rage turns count to 10" in new TestContext {
      val barbarian = random[Barbarian]
        .withRageUsagesLeft(2)
        .withRageTurnsLeft(5)
        .withCombatIndex(1)

      val (Combatant(_, ragingBarbarian: Barbarian), _) =
        rage(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      ragingBarbarian.rageTurnsLeft shouldBe 10
    }

    /* Duplicates appear so that when the resistances are removed the Barbarian retains those it already had  */
    "add resistance to Bludgeoning, Piercing and Slashing damage" in new TestContext {
      val barbarian = random[Barbarian].withResistance(Fire, Slashing).withCombatIndex(1)

      val (Combatant(_, ragingBarbarian: Barbarian), _) =
        rage(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      ragingBarbarian.resistances shouldBe List(Fire, Slashing, Bludgeoning, Piercing, Slashing)
    }

    "use the Barbarian's bonus action" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val (Combatant(_, ragingBarbarian: Barbarian), _) =
        rage(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      ragingBarbarian.bonusActionUsed shouldBe true
    }
  }

  "Reckless Attack" should {

    "set the Barbarian's attackStatus to Advantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val (Combatant(_, recklessBarbarian: Barbarian), _) =
        recklessAttack(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      recklessBarbarian.attackStatus shouldBe Advantage
    }

    "set the Barbarian's defenseStatus to Disadvantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val (Combatant(_, recklessBarbarian: Barbarian), _) =
        recklessAttack(Priority)(barbarian).useAbility(List.empty[Combatant], LowestFirst)

      recklessBarbarian.defenseStatus shouldBe Disadvantage
    }

    "always use a regular attack after RecklessAttack" in {
      forAll { (barbarian: Barbarian, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val trackedBarbarian = barbarian
            .withAbilities(List(recklessAttack(1), trackedAbility(2)))
            .withBaseWeapon(trackedSword)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          recklessAttack(Priority)(trackedBarbarian).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
          trackedAbilityUsedCount shouldBe 0
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var swordUsedCount = 0
    val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      swordUsedCount += 1
      1
    })

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false
    def trackedAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-one"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = WholeAction

        def triggerMet(others: List[Combatant])   = true
        def conditionMet: Boolean = trackedAbilityUsed == false

        def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
          trackedAbilityUsedCount += 1
          (combatant, others)
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

        def triggerMet(others: List[Combatant])   = true
        def conditionMet: Boolean = trackedBonusActionUsed == false

        def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
          trackedBonusActionUsedCount += 1
          (combatant, others)
        }

        def update: Creature = {
          trackedBonusActionUsed = true
          combatant.creature
        }
      }
  }
}
