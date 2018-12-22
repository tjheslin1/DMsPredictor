package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes._
import io.github.tjheslin1.dmspredictor.equipment.armour.ChainShirt
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue

class FighterSpec extends UnitSpecBase {

  implicit val roll = Dice.defaultRandomiser

  "Fighter" should {
    "use Second Wind when it has reached a health threshold" in {
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
    "calculate starting health for level one fighter with default constitution score" in {
      Fighter.calculateHealth(LevelOne, 10) shouldBe 10
    }

    "calculate starting health for level one fighter with low constitution score" in {
      Fighter.calculateHealth(LevelOne, 6) shouldBe 8
    }

    "calculate starting health for level one fighter with high constitution score" in {
      Fighter.calculateHealth(LevelOne, 16) shouldBe 13
    }

    "calculate health for level two fighter with default constitution score" in {
      Fighter.calculateHealth(LevelTwo, 10) shouldBe 16
    }

    "calculate health for level twenty fighter with high constitution score" in {
      Fighter.calculateHealth(LevelTwenty, 19) shouldBe 204
    }
  }

  "weapon" should {
    "apply +2 to hit bonus for a melee weapon with the Dueling fighting style" in {
      val sword = Weapon("sword", Melee, Slashing, 10)

      forAll { fighter: Fighter =>
        val meleeFighter = fighter.copy(baseWeapon = sword).withFightingStyle(Dueling)

        meleeFighter.weapon.hitBonus shouldBe 2
      }
    }

    "apply +2 to hit bonus for a ranged weapon with the Archery fighting style" in {
      val bow = Weapon("bow", Ranged, Piercing, 10)

      forAll { fighter: Fighter =>
        val rangedFighter = fighter.copy(baseWeapon = bow).withFightingStyle(Archery)

        rangedFighter.weapon.hitBonus shouldBe 2
      }
    }

    "apply no hit bonus for a weapon without a complementary fighting style" in {
      val sword = Weapon("sword", Melee, Slashing, 10)

      forAll { fighter: Fighter =>
        val noStyleFighter = fighter.copy(baseWeapon = sword).withNoFightingStyles()

        noStyleFighter.weapon.hitBonus shouldBe 0
      }
    }
  }

  "armourClass" should {
    "calculate default armour class for no armour and no shield" in {
      forAll { fighter: Fighter =>
        val unarmouredFighter = fighter.withNoArmour().withNoShield().withNoFightingStyles().withDexterity(12)

        unarmouredFighter.armourClass shouldBe 11
      }
    }

    "calculate armour class for wearing armour but no shield" in {
      forAll { fighter: Fighter =>
        val armouredFighter = fighter.withArmour(ChainShirt).withNoShield().withNoFightingStyles().withDexterity(10)

        armouredFighter.armourClass shouldBe 13
      }
    }

    "calculate armour class for wearing a shield but no armour" in {
      forAll { fighter: Fighter =>
        val shieldedFighter = fighter.withNoArmour().withShield().withNoFightingStyles().withDexterity(10)

        shieldedFighter.armourClass shouldBe 12
      }
    }

    "calculate armour class for wearing armour and a shield" in {
      forAll { fighter: Fighter =>
        val armouredAndShieldedFighter = fighter.withArmour(ChainShirt).withShield().withNoFightingStyles().withDexterity(10)

        armouredAndShieldedFighter.armourClass shouldBe 15
      }
    }

    "calculate armour class for wearing armour, shield and with high dexterity" in {
      forAll { fighter: Fighter =>
        val armouredAndShieldedFighter = fighter.withArmour(ChainShirt).withShield().withNoFightingStyles().withDexterity(14)

        armouredAndShieldedFighter.armourClass shouldBe 17
      }
    }

    "calculate armour class for having armour and the Defense fighting style" in {
      forAll { fighter: Fighter =>
        val defensiveFighter = fighter.withArmour(ChainShirt).withNoShield().withFightingStyle(Defense).withDexterity(10)

        defensiveFighter.armourClass shouldBe 14
      }
    }

    "calculate armour class for having no armour and ignoring Defense fighting style" in {
      forAll { fighter: Fighter =>
        val defensiveFighter = fighter.withNoArmour().withNoShield().withFightingStyle(Defense).withDexterity(10)

        defensiveFighter.armourClass shouldBe 10
      }
    }
  }
}
