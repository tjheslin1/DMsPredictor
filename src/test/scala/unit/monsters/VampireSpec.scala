package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Grappled
import io.github.tjheslin1.dmspredictor.monsters.Vampire._
import io.github.tjheslin1.dmspredictor.monsters.{MonsterAbilities, Vampire}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class VampireSpec extends UnitSpecBase {

  "updateHealth" should {
    "set radiantDamageTaken to true if Radiant damage taken" in
      forAll { vampire: Vampire =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val healthyVampire = vampire.withHealth(50).withMaxHealth(100)

          val updatedVampire = healthyVampire.updateHealth(10, Radiant, Hit).asInstanceOf[Vampire]

          updatedVampire.radiantDamageTaken shouldBe true
        }
      }
  }

  "resetStartOfTurn" should {
    "regenerate 20 hit points if the Vampire hasn't taken Radiant damage last turn" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire = vampire.withHealth(50).withMaxHealth(100).resetStartOfTurn()

        regeneratedVampire.health shouldBe 70
      }
    }

    "not regenerate hit points if the Vampire has taken Radiant damage last turn" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire =
          vampire
            .copy(radiantDamageTaken = true)
            .withHealth(50)
            .withMaxHealth(100)
            .resetStartOfTurn()

        regeneratedVampire.health shouldBe 50
      }
    }

    "reset radiantDamageTaken to false" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire = vampire
          .copy(radiantDamageTaken = true)
          .resetStartOfTurn()
          .asInstanceOf[Vampire]

        regeneratedVampire.radiantDamageTaken shouldBe false
      }
    }

    "reset BiteUsed to false" in {
      forAll { vampire: Vampire =>
        val biteVampire = vampire
          .copy(biteUsed = true)
          .resetStartOfTurn()
          .asInstanceOf[Vampire]

        biteVampire.radiantDamageTaken shouldBe false
      }
    }
  }

  "Vampire" should {
    "make one regular attack and one Bite attack when using MultiAttack" in {
      forAll { (vampire: Vampire, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val vampireCombatant =
            vampire.withStrength(20).withBaseWeapon(trackedSword).withCombatIndex(1)
          val fighterCombatant = fighter.withDexterity(1).withArmourClass(1).withCombatIndex(1)

          val (Combatant(_, updatedVampire: Vampire), _) = MonsterAbilities
            .multiAttack(1, 2)(vampireCombatant)
            .useAbility(List(fighterCombatant), LowestFirst)

          updatedVampire.biteUsed shouldBe true
          swordUsedCount shouldBe 1
        }
      }
    }
  }

  "UnarmedStrike" should {
    "Grapple a creature" in {
      fail("todo")
    }
  }

  "bite" should {
    "target a creature who is grappled" in {
      forAll { (vampire: Vampire, fighter: Fighter, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val vampireCombatant = vampire.withStrength(20).withCombatIndex(1)
          val fighterCombatant = fighter.withStrength(1).withDexterity(1).withCombatIndex(2)
          val clericCombatant  = cleric.withCondition(Grappled(18)).withStrength(1).withDexterity(1).withCombatIndex(3)

          val (Combatant(_, updatedVampire: Vampire),
               List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedCleric: Cleric))) =
            bite(1)(vampireCombatant)
              .useAbility(List(fighterCombatant, clericCombatant), LowestFirst)


        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var swordUsedCount = 0
    val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      swordUsedCount += 1
      1
    })
  }
}
