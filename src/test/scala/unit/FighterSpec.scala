package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes._
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.equipment.armour.ChainShirt
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue
import scala.util.Random

class FighterSpec extends UnitSpecBase {

  "Fighter" should {

    "utilise Two Weapon Fighting if equipped with two weapons" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (fighter: Fighter, testMonster: TestMonster) =>
      var swordUsedCount = 0
      val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
        swordUsedCount += 1
        1
      })

      val dualWieldingFighter = fighter
          .copy(secondWindUsed = true)
          .withWeapon(trackedSword)
          .withOffHand(trackedSword)
          .withFightingStyle(TwoWeaponFighting)
          .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        Move.takeMove(Queue(dualWieldingFighter, monster), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }

    "use Second Wind when it has reached a health threshold" in new TestContext {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val lowHealthFighter =
          fighter.withHealth(1).withMaxHealth(5).copy(level = LevelTwo, secondWindUsed = false).withCombatIndex(1)

        val monster = testMonster.withCombatIndex(2)

        val Queue(_, Combatant(_, updatedFighter)) = Move.takeMove(Queue(lowHealthFighter, monster), LowestFirst)

        updatedFighter.health should (be > 1 and be <= 5)
      }
    }
  }

  "calculateHealth" should {
    "calculate starting health for level one fighter with default constitution score" in new TestContext {
      Fighter.calculateHealth(LevelOne, 10) shouldBe 10
    }

    "calculate starting health for level one fighter with low constitution score" in new TestContext {
      Fighter.calculateHealth(LevelOne, 6) shouldBe 8
    }

    "calculate starting health for level one fighter with high constitution score" in new TestContext {
      Fighter.calculateHealth(LevelOne, 16) shouldBe 13
    }

    "calculate health for level two fighter with default constitution score" in new TestContext {
      Fighter.calculateHealth(LevelTwo, 10) shouldBe 16
    }

    "calculate health for level twenty fighter with high constitution score" in new TestContext {
      Fighter.calculateHealth(LevelTwenty, 19) shouldBe 204
    }
  }

  "weapon" should {
    "apply +2 to hit bonus for a one handed melee weapon with the Dueling fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, false, 10)

      forAll { fighter: Fighter =>
        val meleeFighter = fighter.withWeapon(sword).withFightingStyle(Dueling)

        meleeFighter.weapon.hitBonus shouldBe 2
      }
    }

    "apply +2 to hit bonus for a ranged weapon with the Archery fighting style" in new TestContext {
      val bow = Weapon("bow", Ranged, Piercing, true, 10)

      forAll { fighter: Fighter =>
        val rangedFighter = fighter.withWeapon(bow).withFightingStyle(Archery)

        rangedFighter.weapon.hitBonus shouldBe 2
      }
    }

    "apply no hit bonus for a weapon without a complementary fighting style" in new TestContext {
      val sword = Weapon("sword", Melee, Slashing, true, 10)

      forAll { fighter: Fighter =>
        val noStyleFighter = fighter.withWeapon(sword).withNoFightingStyles()

        noStyleFighter.weapon.hitBonus shouldBe 0
      }
    }

    "reroll a roll of 1 or 2 for a two-handed with the Great Weapon Fighting style" in new TestContext {
      forAll { fighter: Fighter =>
        var count = 0
        val twoHandedWeapon = Weapon("sword", Melee, Slashing, true, {
          count += 1
          D6.roll()(_ => Random.nextInt(2) + 1)
        })

        val twoHanderFighter = fighter.withWeapon(twoHandedWeapon).withFightingStyle(GreatWeaponFighting)

        twoHanderFighter.weapon

        count shouldBe 2
      }
    }
  }

  "armourClass" should {
    "calculate default armour class for no armour and no shield" in new TestContext {
      forAll { fighter: Fighter =>
        val unarmouredFighter = fighter.withNoArmour().withNoShield().withNoFightingStyles().withDexterity(12)

        unarmouredFighter.armourClass shouldBe 11
      }
    }

    "calculate armour class for wearing armour but no shield" in new TestContext {
      forAll { fighter: Fighter =>
        val armouredFighter = fighter.withArmour(ChainShirt).withNoShield().withNoFightingStyles().withDexterity(10)

        armouredFighter.armourClass shouldBe 13
      }
    }

    "calculate armour class for wearing a shield but no armour" in new TestContext {
      forAll { fighter: Fighter =>
        val shieldedFighter = fighter.withNoArmour().withShield().withNoFightingStyles().withDexterity(10)

        shieldedFighter.armourClass shouldBe 12
      }
    }

    "calculate armour class for wearing armour and a shield" in new TestContext {
      forAll { fighter: Fighter =>
        val armouredAndShieldedFighter =
          fighter.withArmour(ChainShirt).withShield().withNoFightingStyles().withDexterity(10)

        armouredAndShieldedFighter.armourClass shouldBe 15
      }
    }

    "calculate armour class for wearing armour, shield and with high dexterity" in new TestContext {
      forAll { fighter: Fighter =>
        val armouredAndShieldedFighter =
          fighter.withArmour(ChainShirt).withShield().withNoFightingStyles().withDexterity(14)

        armouredAndShieldedFighter.armourClass shouldBe 17
      }
    }

    "calculate armour class for having armour and the Defense fighting style" in new TestContext {
      forAll { fighter: Fighter =>
        val defensiveFighter =
          fighter.withArmour(ChainShirt).withNoShield().withFightingStyle(Defense).withDexterity(10)

        defensiveFighter.armourClass shouldBe 14
      }
    }

    "calculate armour class for having no armour and ignoring Defense fighting style" in new TestContext {
      forAll { fighter: Fighter =>
        val defensiveFighter = fighter.withNoArmour().withNoShield().withFightingStyle(Defense).withDexterity(10)

        defensiveFighter.armourClass shouldBe 10
      }
    }
  }

  private class TestContext {
    implicit val roll = Dice.defaultRandomiser
  }
}
