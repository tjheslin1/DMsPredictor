package unit.paladin

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import util.TestData._
import util.TestMonster

class PaladinSpellsSpec extends UnitSpecBase {

  "Bless" should {
    "add 1d4 to weapon attack roll" in {
      forAll { (champion: Champion, testMonster: TestMonster) =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val blessedChampion = champion
            .withNoFightingStyles()
            .withProficiencyBonus(1)
            .withCondition(BlessCondition())
            .withBaseWeapon(trackedSword)
            .withStrength(7)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (attackResult, _) = Actions.attack(blessedChampion, trackedSword, monster)

          println(attackResult)

          attackResult shouldBe Hit
        }
      }
    }

    "add 1d4 to spell attack roll" in {
      fail("TODO")
    }

    "add 1d4 to a saving throw roll" in {
      fail("TODO")
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
