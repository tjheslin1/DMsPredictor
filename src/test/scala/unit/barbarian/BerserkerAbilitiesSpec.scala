package unit.barbarian

import base.UnitSpecBase
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Berserker
import io.github.tjheslin1.dmspredictor.classes.barbarian.BerserkerAbilities._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, BonusAction, WholeAction}
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import util.TestData._
import util.TestMonster

class BerserkerAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "frenzy" should {

    "delegate to the next Action ability" in {
      forAll { (berserker: Berserker, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val trackedBerserker = berserker
            .withAbilities(List(frenzy(1), trackedBonusAction(2), trackedAbility(3)))
            .withCombatIndex(1)

          val monster = testMonster.withCombatIndex(2)

          frenzy(Priority)(trackedBerserker).useAbility(List(monster), LowestFirst)

          trackedAbilityUsedCount shouldBe 1
        }
      }
    }

    "update the barbarian's number of rages left" in new TestContext {
      val berserker = random[Berserker].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.rageUsages shouldBe 1
    }

    "update the barbarian's inFrenzy to true" in new TestContext {
      val frenziesBerserker = random[Berserker].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(frenziesBerserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.inFrenzy shouldBe true
    }

    "set the rage turns count to 10" in new TestContext {
      val berserker = random[Berserker]
        .withRageUsagesLeft(2)
        .withRageTurnsLeft(5)
        .withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.rageTurnsLeft shouldBe 10
    }

    "add resistance to Bludgeoning, Piercing and Slashing damage" in new TestContext {
      val berserker = random[Berserker].withResistance(Fire).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.resistances shouldBe List(Fire, Bludgeoning, Piercing, Slashing)
    }

    "use the Barbarian's bonus action" in new TestContext {
      val berserker = random[Berserker].withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.bonusActionUsed shouldBe true
    }
  }

  "bonusFrenzyAttack" should {

    "use a single attack on Bonus Action whilst frenzying" in {
      forAll { (berserker: Berserker, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val frenzyingBerserker = berserker
            .withInFrenzy()
            .withBaseWeapon(trackedSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(1).withCombatIndex(2)

          bonusFrenzyAttack(Priority)(frenzyingBerserker).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
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
