package unit.monsters

import base.UnitSpecBase
import com.apple.concurrent.Dispatch.Priority
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Grappled
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.vampire.VampireAbilities._
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import util.TestData._

class VampireAbilitiesSpec extends UnitSpecBase {

  "bite" should {
    "target a creature who is grappled" in {
      forAll { (vampire: Vampire, fighter: Fighter, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val vampireCombatant = vampire.withStrength(20).withCombatIndex(1)
          val fighterCombatant = fighter.withStrength(1).withDexterity(1).withCombatIndex(2)

          val clericCombatant =
            cleric
              .withCondition(Grappled(18))
              .withNoArmour()
              .withStrength(1)
              .withDexterity(1)
              .withCombatIndex(3)

          val (_, List(Combatant(_, updatedCleric: Cleric))) =
            bite(1)(vampireCombatant)
              .useAbility(List(fighterCombatant, clericCombatant), LowestFirst)

          updatedCleric.health < cleric.health
        }
      }
    }

    "restore the Vampires health equal to the necrotic damage dealt" in {
      forAll { (vampire: Vampire, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val vampireCombatant =
            vampire.withHealth(50).withMaxHealth(100).withStrength(20).withCombatIndex(1)

          val clericCombatant =
            cleric
              .withCondition(Grappled(18))
              .withNoArmour()
              .withStrength(1)
              .withDexterity(1)
              .withCombatIndex(3)

          val (Combatant(_, updatedVampire: Vampire), _) =
            bite(1)(vampireCombatant)
              .useAbility(List(clericCombatant), LowestFirst)

          updatedVampire.health shouldBe 65
        }
      }
    }

    "reduce the creatures maxHealth (and health accordingly) equal to the necrotic damage taken" in {
      forAll { (vampire: Vampire, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val vampireCombatant =
            vampire.withHealth(50).withMaxHealth(100).withStrength(20).withCombatIndex(1)

          val clericCombatant =
            cleric
              .withHealth(50)
              .withMaxHealth(60)
              .withCondition(Grappled(18))
              .withNoArmour()
              .withStrength(1)
              .withDexterity(1)
              .withCombatIndex(2)

          val (_, List(Combatant(_, updatedCleric: Cleric))) =
            bite(1)(vampireCombatant)
              .useAbility(List(clericCombatant), LowestFirst)

          val expectedPiercingDamage = 10
          val expectedNecroticDamage = 15

          updatedCleric.health shouldBe 50 - expectedPiercingDamage - expectedNecroticDamage
          updatedCleric.maxHealth shouldBe 60 - expectedNecroticDamage
        }
      }
    }
  }

  "unarmedStrike" should {
    "update the Vampires firstAttack to false" in {
      forAll { vampire: Vampire =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val vampireCombatant = vampire.withCombatIndex(1)

          val updatedVampire = unarmedStrike(1)(vampireCombatant).update.asInstanceOf[Vampire]

          updatedVampire.firstAttack shouldBe false
        }
      }
    }

    "attempt to grapple a target instead of dealing damage on its first attack" in {
      forAll { (vampire: Vampire, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val vampireCombatant = vampire.withStrength(20).withCombatIndex(1)

          val clericCombatant = cleric.withNoArmour().withStrength(1).withDexterity(1).withCombatIndex(2)

          val (_, List(Combatant(_, updatedCleric: Cleric))) =
          unarmedStrike(1)(vampireCombatant).useAbility(List(clericCombatant), LowestFirst)

          updatedCleric.conditions shouldBe List(Grappled(18))
        }
      }
    }

    "perform a regular damaging attack on the Vampires second attack" in {
      forAll { (vampire: Vampire, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val vampireCombatant = vampire.copy(firstAttack = false).withStrength(20).withCombatIndex(1)

          val clericCombatant = cleric.withNoArmour().withStrength(1).withDexterity(1).withCombatIndex(2)

          val (_, List(Combatant(_, updatedCleric: Cleric))) =
          unarmedStrike(1)(vampireCombatant).useAbility(List(clericCombatant), LowestFirst)

          updatedCleric.conditions shouldBe List()
          updatedCleric.health < cleric.health
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
