package unit.ranger

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BaseRangerSpec extends UnitSpecBase {

  "calculateHealth" should {
    "calculate starting health for level one fighter with default constitution score" in new TestContext {
      calculateHealth(LevelOne, 10) shouldBe 10
    }

    "calculate starting health for level one fighter with low constitution score" in new TestContext {
      calculateHealth(LevelOne, 6) shouldBe 8
    }

    "calculate starting health for level one fighter with high constitution score" in new TestContext {
      calculateHealth(LevelOne, 16) shouldBe 13
    }

    "calculate health for level two fighter with default constitution score" in new TestContext {
      calculateHealth(LevelTwo, 10) shouldBe 16
    }

    "calculate health for level twenty fighter with high constitution score" in new TestContext {
      calculateHealth(LevelTwenty, 19) shouldBe 204
    }
  }

  "weaponWithFightingStyle" should {
    "apply +2 to hit bonus for a one handed melee weapon with the Dueling fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(sword, List(Dueling)).hitBonus shouldBe 2
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
