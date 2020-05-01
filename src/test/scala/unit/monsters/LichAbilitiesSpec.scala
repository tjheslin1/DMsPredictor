package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.{Archery, Fighter}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Paralyzed}
import io.github.tjheslin1.dmspredictor.monsters.lich.LichAbilities.paralyzingTouch
import io.github.tjheslin1.dmspredictor.monsters.lich._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class LichAbilitiesSpec extends UnitSpecBase {

  "paralyzingTouch" should {
    "deal 3d6 damage" in new TestContext {

      implicit val rollStrategy = _ => RollResult(6)

      val lich = random[Lich].withCombatIndex(1)

      val lowDefenseFighter = random[Fighter]
        .withFightingStyle(Archery)
        .withNoOffHand()
        .withDexterity(10)
        .withHealth(50)
        .withMaxHealth(50)
        .withCombatIndex(2)

      val (_, List(Combatant(_, updatedFighter: Fighter))) =
        paralyzingTouch(1)(lich).useAbility(List(lowDefenseFighter), LowestFirst)

      updatedFighter.health shouldBe 32 // 50 - 3d6
    }

    "paralyze target on hit if saving throw failed" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val lowConstitutionFighter = fighter
              .withNoArmour()
              .withNoOffHand()
              .withDexterity(10)
              .withConstitution(2)
              .withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            paralyzingTouch(1)(lich.withCombatIndex(1)).useAbility(List(lowConstitutionFighter), LowestFirst)

          updatedFighter.conditions should contain theSameElementsAs List(Paralyzed(18, 10, Constitution))
        }
      }
    }

    "not paralyze target on hit if saving throw passed" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(15)

          val highConstitutionFighter = fighter
              .withConstitution(24)
              .withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            paralyzingTouch(1)(lich.withCombatIndex(1)).useAbility(List(highConstitutionFighter), LowestFirst)

          updatedFighter.conditions should contain theSameElementsAs List.empty[Condition]
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
