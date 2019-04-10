package unit.rogue

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogueAbilities._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

class BaseRogueAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "sneakAttack" should {
    "apply sneak attack damage" in {
      forAll { (rogue: Rogue, goblin: Goblin) =>
        new TestContext {
          val diceRolls = Iterator(
            1,
            15, // attack roll with advantage
            1, // sneak damage roll
            1 // weapon damage roll
          )

          implicit override val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val healthyGoblin = goblin.withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val sneakingRogue = rogue
            .isHiddenFrom(List(healthyGoblin))
            .withDexterity(12)
            .withStrength(10)
            .withBaseWeapon(
              Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = true, dmg = 1))
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            sneakAttack(Priority)(sneakingRogue).useAbility(List(healthyGoblin), LowestFirst)

          updatedGoblin.health shouldBe 47
        }
      }
    }

    "reveal the rogue" in {
      forAll { (rogue: Rogue, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(15)

          val sneakingRogue = rogue
            .isHiddenFrom(List(goblin.withCombatIndex(2)))
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val updatedRogue = sneakAttack(Priority)(sneakingRogue).update.asInstanceOf[Rogue]

          updatedRogue.hiddenFrom shouldBe List()
        }
      }
    }
  }

  "hide" should {
    "use the rogues bonus action" in {
      new TestContext {
        implicit override val roll: RollStrategy = _ => RollResult(19)

        val rogue = random[Rogue].withLevel(LevelTwo).withCombatIndex(1)

        val updatedRogue = hide(Priority)(rogue).update.asInstanceOf[Rogue]

        updatedRogue.bonusActionUsed shouldBe true
      }
    }

    "apply the unseen condition to the rogue" in {
      forAll { (rogue: Rogue, goblinOne: Goblin, goblinTwo: Goblin, zombie: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(15)

          val dexterousRogue  = rogue.withProficiencyBonus(2).withDexterity(12).withCombatIndex(1)
          val goblinCombatant = goblinOne.withWisdom(10).withCombatIndex(2)
          val unwiseGoblin    = goblinTwo.withWisdom(2).withCombatIndex(3)
          val wiseZombie      = zombie.withWisdom(20).withCombatIndex(4)

          val (Combatant(_, updatedRogue: Rogue), _) =
            hide(Priority)(dexterousRogue)
              .useAbility(List(goblinCombatant, unwiseGoblin, wiseZombie), LowestFirst)

          updatedRogue.hiddenFrom shouldBe List(goblinCombatant, unwiseGoblin)
        }
      }
    }
  }

  "Two Weapon Fighting" should {

    "be used if Player is equipped with two weapons" in {
      forAll { (rogue: Rogue, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val dualWieldingRogue = rogue
            .withBaseWeapon(trackedSword)
            .withOffHand(trackedOffHandSword)
            .withDexterity(20)
            .withCombatIndex(1)

          val monster = testMonster.withHealth(50).withArmourClass(5).withCombatIndex(2)

          twoWeaponFighting(Priority)(dualWieldingRogue).useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 1
          offHAndSwordUsedCount shouldBe 1
        }
      }
    }

    "set the player's bonus action to be used" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(19)

      val updatedRogue =
        twoWeaponFighting(Priority)(random[Rogue].withCombatIndex(1)).update
          .asInstanceOf[Rogue]

      updatedRogue.bonusActionUsed shouldBe true
    }

    "meet the condition if the Player wields two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRogue = random[Rogue]
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRogue).conditionMet shouldBe true
    }

    "not meet the condition if the Player does not wield two weapons" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val rogue = random[Rogue]
        .withBaseWeapon(trackedSword)
        .withNoOffHand()
        .withLevel(LevelFive)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(rogue).conditionMet shouldBe false
    }

    "not meet the condition if the Player has already used their bonus action this turn" in new TestContext {
      implicit override val roll: RollStrategy = Dice.defaultRandomiser

      val dualWieldingRogue = random[Rogue]
        .withBonusActionUsed()
        .withBaseWeapon(trackedSword)
        .withOffHand(trackedOffHandSword)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      twoWeaponFighting(Priority)(dualWieldingRogue).conditionMet shouldBe false
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
