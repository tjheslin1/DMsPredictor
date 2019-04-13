package unit.rogue

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogueAbilities._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

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
      forAll { (rogue: Rogue, goblinOne: Goblin, goblinTwo: Goblin, vampire: Vampire) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterousRogue = rogue.withStealthScore(2).withCombatIndex(1)
          val goblinCombatant = goblinOne.withCombatIndex(2)
          val unwiseGoblin = goblinTwo.withCombatIndex(3)
          val perceptiveVampire = vampire.withCombatIndex(4)

          val (Combatant(_, updatedRogue: Rogue), _) =
            hide(Priority)(dexterousRogue)
              .useAbility(List(goblinCombatant, unwiseGoblin, perceptiveVampire), LowestFirst)

          updatedRogue.hiddenFrom shouldBe List(goblinCombatant, unwiseGoblin)
        }
      }
    }

      "use the rogues stealth skill" in {}

      "use the enemies passive perception score" in {}
    }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
