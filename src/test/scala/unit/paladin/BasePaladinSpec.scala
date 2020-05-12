package unit.paladin

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladin._
import io.github.tjheslin1.dmspredictor.model._

class BasePaladinSpec extends UnitSpecBase {

  "calculateHealth" should {
    "calculate starting health for level one paladin with default constitution score" in new TestContext {
      calculateHealth(LevelOne, 10) shouldBe 10
    }

    "calculate starting health for level one paladin with low constitution score" in new TestContext {
      calculateHealth(LevelOne, 6) shouldBe 8
    }

    "calculate starting health for level one paladin with high constitution score" in new TestContext {
      calculateHealth(LevelOne, 16) shouldBe 13
    }

    "calculate health for level two paladin with default constitution score" in new TestContext {
      calculateHealth(LevelTwo, 10) shouldBe 16
    }

    "calculate health for level twenty paladin with high constitution score" in new TestContext {
      calculateHealth(LevelTwenty, 19) shouldBe 204
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
