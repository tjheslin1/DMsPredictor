package unit.paladin

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladin._
import io.github.tjheslin1.dmspredictor.classes.paladin._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.model._
import util.TestMonster
import util.TestData._

import scala.util.Random

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

  "weaponWithFightingStyle" should {
    "apply +2 to hit bonus for a one handed melee weapon with the Dueling fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(sword, List(Dueling)).hitBonus shouldBe 2
    }

    "not apply a hit bonus for a two handed melee weapon with the Dueling fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, isTwoHanded = true, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(sword, List(Dueling)).hitBonus shouldBe 0
    }

    "apply no hit bonus for a weapon without a complementary fighting style" in new TestContext {
      val bow = Weapon("bow", Ranged, Piercing, isTwoHanded = true, isFinesse = false, dmg = 10)

      weaponWithFightingStyle(bow, List(Dueling)).hitBonus shouldBe 0
    }

    "reroll a roll of 1 or 2 for a two-handed with the Great Weapon Fighting style" in new TestContext {
      forAll { (paladin: Paladin, testMonster: TestMonster) =>
        var count = 0
        val twoHandedWeapon = Weapon("sword", Melee, Slashing, isTwoHanded = true, isFinesse = false, {
          count += 1
          D6.roll()(_ => Random.nextInt(2) + 1)
        })

        val twoHanderPaladin = paladin.withFightingStyle(GreatWeaponFighting).withBaseWeapon(twoHandedWeapon)

        Actions.resolveDamageMainHand(twoHanderPaladin.withCombatIndex(1), testMonster.withCombatIndex(2), List(), Hit)

        count shouldBe 2
      }
    }
  }

  "armourClass" should {
    "calculate default armour class for no armour and no shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        none[Equipment],
        List.empty[PaladinFightingStyle]
      ) shouldBe 10
    }

    "calculate armour class for wearing armour but no shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainMail,
        none[Equipment],
        List.empty[PaladinFightingStyle]
      ) shouldBe 16
    }

    "calculate armour class for wearing a shield but no armour" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        Shield.some,
        List.empty[PaladinFightingStyle]
      ) shouldBe 12
    }

    "calculate armour class for wearing armour and a shield" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainMail,
        Shield.some,
        List.empty[PaladinFightingStyle]
      ) shouldBe 18
    }

    "calculate armour class for wearing armour, shield and with high dexterity" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 16, 10, 10, 10, 10),
        ChainShirt,
        Shield.some,
        List.empty[PaladinFightingStyle]
      ) shouldBe 17
    }

    "calculate armour class for having armour and the Defense fighting style" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        ChainMail,
        none[Equipment],
        List(Defense)
      ) shouldBe 17
    }

    "calculate armour class for having no armour and ignoring Defense fighting style" in new TestContext {
      armourClassWithFightingStyle(BaseStats(10, 10, 10, 10, 10, 10),
        NoArmour,
        none[Equipment],
        List(Defense)
      ) shouldBe 10
    }
  }

  "layOnHandsPoolForLevel" should {
    "have a pool equal to the Paladin's level multiplied by 5" in {
      forAll { paladin: Paladin =>

        layOnHandsPoolForLevel(paladin.level) shouldBe paladin.level.value * 5
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
