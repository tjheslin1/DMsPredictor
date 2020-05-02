package unit.ranger

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.{Defense, Ranger}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import util.TestData._

class RangerSpec extends UnitSpecBase {

  "updateHealth" should {

      "set the Ranger to dead if the damage brings health below negative max health" in new TestContext {
        override implicit val roll: RollStrategy = _ => RollResult(10)

        val ranger = random[Ranger]
          .withHealth(50)
          .withMaxHealth(50)

        val updatedRanger = ranger.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Ranger]

        updatedRanger.isAlive shouldBe false
      }

    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingRanger = random[Ranger]
        .withSpellKnown(HuntersMark)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)

      val updatedRanger = concentratingRanger.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Ranger]

      updatedRanger.concentratingSpell shouldBe HuntersMark.some
    }

    "handle loss of concentration if ranger goes unconscious" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingRanger = random[Ranger]
        .withSpellKnown(HuntersMark)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)
        .withHealth(1)
        .withMaxHealth(50)

      val updatedRanger = concentratingRanger.updateHealth(1, Bludgeoning, Hit).asInstanceOf[Ranger]

      updatedRanger.concentratingSpell shouldBe none[Spell]
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
