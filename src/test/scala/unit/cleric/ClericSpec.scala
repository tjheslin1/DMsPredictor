package unit.cleric

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
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
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
