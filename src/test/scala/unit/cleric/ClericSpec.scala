package unit.cleric

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SpiritGuardians
import util.TestData._

class ClericSpec extends UnitSpecBase {

  "updateHealth" should {
    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingCleric = random[Cleric]
        .withConcentratingOn(SpiritGuardians)
        .withConstitution(2)

      val updatedCleric = concentratingCleric.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Cleric]

      updatedCleric.concentratingSpell shouldBe SpiritGuardians.some
    }

    "handle loss of concentration if cleric goes unconscious" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingCleric = random[Cleric]
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withConcentratingOn(SpiritGuardians)
        .withLevel(LevelFive)
        .withConstitution(2)
        .withHealth(1)
        .withMaxHealth(50)

      val updatedCleric = concentratingCleric.updateHealth(1, Bludgeoning, Hit).asInstanceOf[Cleric]

      updatedCleric.concentratingSpell shouldBe none[Spell]
    }

    "set the Cleric to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedCleric = cleric.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Cleric]

      updatedCleric.isAlive shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
