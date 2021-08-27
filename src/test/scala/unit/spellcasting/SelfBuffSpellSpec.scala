package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import util.TestData._

class SelfBuffSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply buff condition to caster" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(19)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)

          val buffingRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellKnown(trackedBuffSpell)
            .withLevel(LevelTwo)
            .asInstanceOf[Ranger]

          val (updatedRanger: Ranger, _) = trackedBuffSpell.effect(
            buffingRanger,
            HuntersMark.spellLevel,
            List.empty[Combatant])

          updatedRanger.conditions shouldBe List(HuntersMarkBuffCondition)
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
