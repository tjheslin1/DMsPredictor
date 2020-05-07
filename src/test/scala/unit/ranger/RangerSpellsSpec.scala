package unit.ranger

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
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
    "meet the condition if the Player has the HuntersMarkBuffCondition, knows the HuntersMark spell and meets the level requirement" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedRanger = ranger
            .withSpellKnown(HuntersMark)
            .withLevel(LevelTwo)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedRanger).conditionMet shouldBe true
        }
      }
    }

    "not meet the condition if the Player does not meet the level requirement" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedRanger = ranger
            .withSpellKnown(HuntersMark)
            .withLevel(LevelOne)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedRanger).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the Player does not have the HuntersMarkBuffCondition" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedRanger = ranger
            .withSpellKnown(HuntersMark)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedRanger).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the Player is not a high enough level" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedRanger = ranger
            .withSpellKnown(HuntersMark)
            .withLevel(LevelOne)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedRanger).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the Player does not know the HuntersMark spell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedRanger = ranger
            .withSpellKnown(CureWounds)
            .withLevel(LevelTwo)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedRanger).conditionMet shouldBe false
        }
      }
    }

    "meet the condition if the SpellCaster (Monster) has the HuntersMarkBuffCondition knows the HuntersMark spell" in {
      forAll { lich: Lich =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedLich = lich
            .withSpellKnown(HuntersMark)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedLich).conditionMet shouldBe true
        }
      }
    }

    "not meet the condition if the SpellCaster (Monster) does not know the HuntersMark spell" in {
      forAll { lich: Lich =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedLich = lich
            .withSpellKnown(CureWounds)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedLich).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the SpellCaster (Monster) does not have the HuntersMarkBuffCondition" in {
      forAll { lich: Lich =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedLich = lich
            .withSpellKnown(HuntersMark)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedLich).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the creature is not a SpellCaster" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val buffedFighter = fighter
            .withLevel(LevelTwo)
            .withCondition(HuntersMarkBuffCondition)
            .withCombatIndex(1)

          huntersMarkOnWeaponDamageAbility(1)(buffedFighter).conditionMet shouldBe false
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
