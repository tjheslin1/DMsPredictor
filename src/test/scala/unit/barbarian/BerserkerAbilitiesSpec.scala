package unit.barbarian

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.classes.barbarian.Berserker
import io.github.tjheslin1.dmspredictor.classes.barbarian.BerserkerAbilities._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
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
            .withAbilities(
              List(frenzy(1), trackedAbility(2, action = BonusAction), trackedAbility(3)))
            .withCombatIndex(1)

          val monster = testMonster.withCombatIndex(2)

          frenzy(Priority)(trackedBerserker).useAbility(List(monster), LowestFirst)

          trackedAbilityUsedCount shouldBe 1
        }
      }
    }

    "update the barbarian's number of rages left" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val berserker = random[Berserker].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.rageUsages shouldBe 1
    }

    "update the barbarian's inFrenzy to true" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val frenziesBerserker = random[Berserker].withRageUsagesLeft(2).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(frenziesBerserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.inFrenzy shouldBe true
    }

    "set the rage turns count to 10" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val berserker = random[Berserker]
        .withRageUsagesLeft(2)
        .withRageTurnsLeft(5)
        .withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.rageTurnsLeft shouldBe 10
    }

    "add resistance to Bludgeoning, Piercing and Slashing damage" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val berserker = random[Berserker].withResistance(Fire).withCombatIndex(1)

      val (Combatant(_, frenzyingBerserker: Berserker), _) =
        frenzy(Priority)(berserker).useAbility(List.empty[Combatant], LowestFirst)

      frenzyingBerserker.resistances shouldBe List(Fire, Bludgeoning, Piercing, Slashing)
    }

    "use the Barbarian's bonus action" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

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
            .withAbilities(List())
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(1).withCombatIndex(2)

          bonusFrenzyAttack(Priority)(frenzyingBerserker).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
        }
      }
    }

    "delegate to a SingleAttack action ability" in {
      forAll { (berserker: Berserker, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val trackedSingleAttack: CombatantAbility =
            trackedAbility(2,
                           action = SingleAttack,
                           updatedTracking = trackedSingleAttackUsed = true)

          val frenzyingBerserker = berserker
            .withInFrenzy()
            .withBaseWeapon(trackedSword)
            .withAbilities(List(bonusFrenzyAttack(1), trackedSingleAttack))
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(1).withCombatIndex(2)

          bonusFrenzyAttack(Priority)(frenzyingBerserker).useAbility(List(monster), LowestFirst)

          trackedSingleAttackUsed shouldBe true
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy

    var trackedSingleAttackUsed = false
  }
}
