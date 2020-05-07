package unit.ranger

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger._
import io.github.tjheslin1.dmspredictor.classes.ranger._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model._

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

    "not apply a hit bonus for a two handed melee weapon with the Dueling fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, isTwoHanded = true, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(sword, List(Dueling)).hitBonus shouldBe 0
    }

    "apply +2 to hit bonus for a ranged weapon with the Archery fighting style" in new TestContext {
      val bow = Weapon("bow", Ranged, Piercing, isTwoHanded = true, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(bow, List(Archery)).hitBonus shouldBe 2
    }

    "apply no hit bonus for a weapon without a complementary fighting style" in new TestContext {
      val bow = Weapon("bow", Ranged, Piercing, isTwoHanded = true, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(bow, List(Dueling)).hitBonus shouldBe 0
    }
  }

  "armourClass" should {
    "calculate default armour class for no armour and no shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        none[Equipment],
        List.empty[RangerFightingStyle]
      ) shouldBe 10
    }

    "calculate armour class for wearing armour but no shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainShirt,
        none[Equipment],
        List.empty[RangerFightingStyle]
      ) shouldBe 13
    }

    "calculate armour class for wearing a shield but no armour" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        Shield.some,
        List.empty[RangerFightingStyle]
      ) shouldBe 12
    }

    "calculate armour class for wearing armour and a shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainShirt,
        Shield.some,
        List.empty[RangerFightingStyle]
      ) shouldBe 15
    }

    "calculate armour class for wearing armour, shield and with high dexterity" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 14, 10, 10, 10, 10),
        ChainShirt,
        Shield.some,
        List.empty[RangerFightingStyle]
      ) shouldBe 17
    }

    "calculate armour class for having armour and the Defense fighting style" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainShirt,
        none[Equipment],
        List(Defense)
      ) shouldBe 14
    }

    "calculate armour class for having no armour and ignoring Defense fighting style" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        none[Equipment],
        List(Defense)
      ) shouldBe 10
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
