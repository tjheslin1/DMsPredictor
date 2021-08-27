package unit.cleric

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseCleric._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{ChainShirt, NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model._

class BaseClericSpec extends UnitSpecBase {

  "calculateHealth" should {
    "calculate health for a level one cleric with average Constitution" in new TestContext {
      calculateHealth(LevelOne, 10) shouldBe 8
    }

    "calculate health for a level one cleric with high Constitution" in new TestContext {
      calculateHealth(LevelOne, 14) shouldBe 10
    }

    "calculate health for a level one cleric with low Constitution" in new TestContext {
      calculateHealth(LevelOne, 6) shouldBe 6
    }
    "calculate health for a level two cleric with average Constitution" in new TestContext {
      calculateHealth(LevelTwo, 10) shouldBe 13
    }
  }

  "armourClass" should {
    "calculate whilst wearing no armour and no shield" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), NoArmour, none[Equipment]) shouldBe 11
    }

    "calculate whilst wielding a shield but wearing no armour" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), NoArmour, Shield.some) shouldBe 13
    }

    "calculate whilst wearing armour with no shield" in new TestContext {
      calculateArmourClass(
        BaseStats(12, 12, 12, 12, 12, 12),
        ChainShirt,
        none[Equipment]) shouldBe 14
    }

    "calculate whilst wielding a shield and wearing armour" in new TestContext {
      calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), ChainShirt, Shield.some) shouldBe 16
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
