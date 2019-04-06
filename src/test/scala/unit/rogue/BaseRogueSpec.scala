package unit.rogue

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue.{calculateArmourClass, calculateHealth}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{ChainShirt, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.{BaseStats, Dice, LevelOne, LevelTwenty, LevelTwo, RollStrategy}

class BaseRogueSpec extends UnitSpecBase {

  "armourClass" should {
    "calculate whilst wearing no armour and no shield" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), NoArmour, none[Equipment]) shouldBe 11
    }

    "calculate whilst wearing armour with no shield" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), ChainShirt, none[Equipment]) shouldBe 14
    }
  }

  "calculateHealth" should {
    "calculate starting health for level one barbarian with default constitution score" in new TestContext {
      calculateHealth(LevelOne, 10) shouldBe 8
    }

    "calculate starting health for level one barbarian with low constitution score" in new TestContext {
      calculateHealth(LevelOne, 6) shouldBe 6
    }

    "calculate starting health for level one barbarian with high constitution score" in new TestContext {
      calculateHealth(LevelOne, 16) shouldBe 11
    }

    "calculate health for level two barbarian with default constitution score" in new TestContext {
      calculateHealth(LevelTwo, 10) shouldBe 13
    }

    "calculate health for level twenty barbarian with high constitution score" in new TestContext {
      calculateHealth(LevelTwenty, 19) shouldBe 183
    }
  }


  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
