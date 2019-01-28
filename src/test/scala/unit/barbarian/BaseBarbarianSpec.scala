package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian.resetStatus
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities.recklessAttack
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BaseBarbarianSpec extends UnitSpecBase {

  "turnReset" should {
    "reset Reckless Attack attackStatus and DefenseStats at start of turn" in new TestContext {
      val barbarian = random[Barbarian]

      val recklessBarbarian =
        recklessAttack(1)(barbarian.withCombatIndex(1)).update.asInstanceOf[Barbarian]

      val newTurnBarbarian = resetStatus(recklessBarbarian)

      newTurnBarbarian.attackStatus shouldBe Regular
      newTurnBarbarian.defenseStatus shouldBe Regular
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
