package unit.barbarian

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model._

class BarbarianSpec extends UnitSpecBase {

  "armourClass" should {
    "calculate whilst wearing no armour and no shield" in new TestContext {
      Barbarian.calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), NoArmour, none[Equipment]) shouldBe 12
    }

    "calculate whilst wielding a shield but wearing no armour" in new TestContext {
      Barbarian.calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), NoArmour, Shield.some) shouldBe 14
    }

    "calculate whilst wearing armour with no shield" in new TestContext {
      Barbarian.calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), ChainShirt, none[Equipment]) shouldBe 14
    }

    "calculate whilst wielding a shield and wearing armour" in new TestContext {
      Barbarian.calculateArmourClass(BaseStats(12, 12, 12, 12, 12, 12), ChainShirt, Shield.some) shouldBe 16
    }
  }

  "weapon" should {
    "apply +2 to damage bonus whilst using Rage ability" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, twoHands = false, 10)

      Barbarian.weaponWithRageDamage(sword, inRage = true).damage shouldBe 12
    }
  }

  "calculateHealth" should {
    "calculate starting health for level one barbarian with default constitution score" in new TestContext {
      Barbarian.calculateHealth(LevelOne, 10) shouldBe 12
    }

    "calculate starting health for level one barbarian with low constitution score" in new TestContext {
      Barbarian.calculateHealth(LevelOne, 6) shouldBe 10
    }

    "calculate starting health for level one barbarian with high constitution score" in new TestContext {
      Barbarian.calculateHealth(LevelOne, 16) shouldBe 15
    }

    "calculate health for level two barbarian with default constitution score" in new TestContext {
      Barbarian.calculateHealth(LevelTwo, 10) shouldBe 19
    }

    "calculate health for level twenty barbarian with high constitution score" in new TestContext {
      Barbarian.calculateHealth(LevelTwenty, 19) shouldBe 225
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
