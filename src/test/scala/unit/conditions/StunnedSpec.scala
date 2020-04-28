package unit.conditions

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Poisoned, Stunned}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue

class StunnedSpec extends UnitSpecBase {

  "handle" should {

    "sustain Stunned condition if saving throw failed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(2)

          val stunned      = Stunned(10)
          val poisoned     = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withConditions(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.conditions should contain theSameElementsAs List(stunned, poisoned)
        }
      }
    }

    "set Defense Status to Disadvantage on saving throw failed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(2)

          val stunned      = Stunned(10)
          val poisoned     = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withConditions(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.defenseStatus shouldBe Disadvantage
        }
      }
    }

    "remove Stunned condition if saving throw passed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val stunned      = Stunned(10)
          val poisoned     = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withConditions(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.conditions should contain theSameElementsAs List(poisoned)
        }
      }
    }

    "set Defense Status to to Regular on saving throw passed" in {
      forAll { rogue: Rogue =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val stunned      = Stunned(10)
          val poisoned     = Poisoned(10, 10)
          val stunnedRogue = rogue.withConstitution(10).withConditions(stunned, poisoned)

          val updatedRogue = stunned.handleEndOfTurn(stunnedRogue)

          updatedRogue.defenseStatus shouldBe Regular
        }
      }
    }
  }

  "Stunned condition" should {

    "automatically fail Strength saving throws" in {
      forAll { champion: Champion =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val stunnedChampion = champion.withCondition(Stunned(10))

          savingThrowPassed(2, Strength, stunnedChampion) shouldBe false
        }
      }
    }

    "automatically fail Dexterity saving throws" in {
      forAll { champion: Champion =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val stunnedChampion = champion.withCondition(Stunned(10))

          savingThrowPassed(2, Dexterity, stunnedChampion) shouldBe false
        }
      }
    }

    "prevent the creature from taking actions and reactions" in {
      // format: off
//      forAll { (champion: Champion, rogue: Rogue, wizard: Wizard, goblin: Goblin,
//                lich: Lich, ranger: Ranger, zombie: Zombie) =>
      forAll { (champion: Champion, rogue: Rogue, wizard: Wizard, goblin: Goblin, lich: Lich) =>
        // format: on
        new TestContext with Tracking {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val stunned = Stunned(20)

          // format: off
          val stunnedChampion = champion.withBaseWeapon(trackedSword)
            .withConstitution(1).withCondition(stunned).withCombatIndex(1)
          val stunnedRogue = rogue.withBaseWeapon(trackedSword)
            .withConstitution(1).withCondition(stunned).withCombatIndex(1)
          val stunnedWizard = wizard.withSpellsKnown(List.empty[Spell]: _*).withBaseWeapon(trackedSword)
              .withConstitution(1).withCondition(stunned).withCombatIndex(1)
          val stunnedGoblin = goblin.withBaseWeapon(trackedSword)
            .withConstitution(1).withCondition(stunned).withCombatIndex(1)
          val stunnedLich = lich.withSpellsKnown(List.empty[Spell]: _*).withBaseWeapon(trackedSword)
              .withConstitution(1).withCondition(stunned).withCombatIndex(1)
          // format: on

          val rangerCombatant = random[Ranger].withCombatIndex(2)
          val zombieCombatant = random[Zombie].withCombatIndex(3)

          val stunnedCombatants =
            List(stunnedChampion, stunnedRogue, stunnedWizard, stunnedGoblin, stunnedLich)

          stunnedCombatants.foreach { stunnedCombatant =>
            Move.takeMove(Queue(stunnedCombatant, rangerCombatant, zombieCombatant), LowestFirst)
          }

          swordUsedCount shouldBe 0
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
