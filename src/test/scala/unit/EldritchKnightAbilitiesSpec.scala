package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class EldritchKnightAbilitiesSpec extends UnitSpecBase {

  "EldritchKnight" should {

    "cast a spell (spell attack) using the highest available spell slot" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        var spellUsedCount = 0

        val trackedMeleeSpellAttack = Spell(1, Evocation, OneAction, MeleeSpellAttack, Fire, {
          spellUsedCount += 1
          1
        })

        val spellCastingEK = eldritchKnight
          .withSpell(trackedMeleeSpellAttack)
          .withAllBaseFighterAbilitiesUsed()
          .withAllSpellSlotsAvailable()

        val eldritchKnightCombatant = spellCastingEK
          .withLevel(LevelThree)
          .withCombatIndex(1)

        val monster = testMonster.withArmourClass(10).withCombatIndex(2)

        val Queue(_, Combatant(_, updatedEK: EldritchKnight)) =
          Move.takeMove(Queue(eldritchKnightCombatant, monster), LowestFirst)

        spellUsedCount shouldBe 1
        updatedEK.spellSlots.firstLevel.count shouldBe (spellCastingEK.spellSlots.firstLevel.count - 1)
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        var spellUsedCount = 0

        val trackedSavingThrowSpell = Spell(1, Evocation, OneAction, SavingThrow(Wisdom), Fire, {
          spellUsedCount += 1
          1
        })

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

        spellUsedCount shouldBe 1
        updatedEK.spellSlots.firstLevel.count shouldBe (spellCastingEK.spellSlots.firstLevel.count - 1)
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
