package unit.fighter

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.EldritchKnightAbilities.castSpell
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class EldritchKnightAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Cast Spell" should {

    "cast a spell (spell attack) using the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val eldritchKnightCombatant = eldritchKnight
            .withSpell(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          castSpell(Priority)(eldritchKnightCombatant).useAbility(monster.some)

          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val spellCastingEK = eldritchKnight
            .withSpell(trackedSavingThrowSpell)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()

          val eldritchKnightCombatant = spellCastingEK
            .withLevel(LevelThree)
            .withIntelligence(10)
            .withProficiencyBonus(6)
            .withCombatIndex(1)

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          val Queue(_, Combatant(_, updatedEK: EldritchKnight)) =
            Move.takeMove(Queue(eldritchKnightCombatant, monster), LowestFirst)

          savingThrowSpellUsedCount shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val spellCastingEK = eldritchKnight
            .withSpell(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()

          val eldritchKnightCombatant = spellCastingEK.withLevel(LevelThree).withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedEldritchKnight: EldritchKnight =
            castSpell(Priority)(eldritchKnightCombatant).update.asInstanceOf[EldritchKnight]

          updatedEldritchKnight.spellSlots.firstLevel.count shouldBe (spellCastingEK.spellSlots.firstLevel.count - 1)
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var meleeSpellUsedCount = 0
    val trackedMeleeSpellAttack = Spell(1, Evocation, OneAction, MeleeSpellAttack, Fire, {
      meleeSpellUsedCount += 1
      1
    })

    var savingThrowSpellUsedCount = 0
    val trackedSavingThrowSpell = Spell(1, Evocation, OneAction, SavingThrow(Wisdom), Fire, {
      savingThrowSpellUsedCount += 1
      1
    })
  }
}
