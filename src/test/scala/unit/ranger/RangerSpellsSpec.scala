package unit.ranger

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import util.TestData._
import util.TestMonster

class RangerSpellsSpec extends UnitSpecBase {

  "HuntersMark" should {
    "apply the HuntersMarkBuffCondition" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(4)

          val levelTwoRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .asInstanceOf[Ranger]

          val monster = testMonster.withCombatIndex(2)

          val (updatedRanger: Ranger, _) =
            HuntersMark.effect(levelTwoRanger, 1, List(monster))

          updatedRanger.conditions shouldBe List(HuntersMarkBuffCondition)
        }
      }
    }

    "be removed if the caster loses concentration" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(4)

          val levelTwoRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withConstitution(4) // to save concentration check
            .withHealth(50)
            .withMaxHealth(50)
            .asInstanceOf[Ranger]

          val (updatedRanger: Ranger, _) =
            HuntersMark.effect(levelTwoRanger, 1, List.empty[Combatant])

          updatedRanger.conditions shouldBe List(HuntersMarkBuffCondition)

          val healthUpdatedRanger = updatedRanger.updateHealth(20, Bludgeoning, Hit)

          healthUpdatedRanger.conditions shouldBe List.empty[Condition]
        }
      }
    }
  }

  "huntersMarkOnWeaponDamageAbility" should {
    "meet the condition if the Player has the HuntersMarkBuffCondition" in {
      fail("TODO")
    }

    "meet the condition if the SpellCaster (Monster) has the HuntersMarkBuffCondition" in {
      fail("TODO")
    }

    "not meet the condition if the Player is not a high enough level" in {
      fail("TODO")
    }

    "not meet the condition if the Player does not know the HuntersMark spell" in {
      fail("TODO")
    }

    "not meet the condition if the SpellCaster (Monster) does not know the HuntersMark spell" in {
      fail("TODO")
    }

    "not meet the condition if the creature is not a SpellCaster" in {
      fail("TODO")
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
