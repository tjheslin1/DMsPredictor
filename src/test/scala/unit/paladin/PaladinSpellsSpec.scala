package unit.paladin

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.{Bless, BlessCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.FireBolt
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import util.TestData._
import util.TestMonster

class PaladinSpellsSpec extends UnitSpecBase {

  "Bless" should {

    "add 1d4 to Players weapon attack roll" in {
      forAll { (champion: Champion, testMonster: TestMonster) =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(4)

          val blessedChampion = champion
            .withNoFightingStyles()
            .withProficiencyBonus(1)
            .withCondition(BlessCondition())
            .withBaseWeapon(trackedSword)
            .withStrength(12)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (attackResult, _) = Actions.attack(blessedChampion, trackedSword, monster)

          attackResult shouldBe Hit
        }
      }
    }

    "add 1d4 to a Monsters weapon attack roll" in {
      forAll { (goblin: Goblin, rogue: Rogue) =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(4)

          val blessedGoblin = goblin
            .withCondition(BlessCondition())
            .withBaseWeapon(trackedSword)
            .withStrength(12)
            .withCombatIndex(1)

          val acElevenRogue = rogue
            .withNoArmour()
            .withDexterity(12)
            .withCombatIndex(2)

          val (attackResult, _) = Actions.attack(blessedGoblin, trackedSword, acElevenRogue)

          attackResult shouldBe Hit
        }
      }
    }

    "add 1d4 to spell attack roll" in {
      forAll { (wizard: Wizard, testMonster: TestMonster) =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val blessedWizard = wizard
            .withSpellsKnown(FireBolt)
            .withProficiencyBonus(1)
            .withCondition(BlessCondition())
            .withIntelligence(7)
            .asInstanceOf[Wizard]

          val monster = testMonster.withArmourClass(10)

          val attackResult = Spell.spellAttack(blessedWizard, monster)

          attackResult shouldBe Hit
        }
      }
    }

    "add 1d4 to a Monsters spell attack roll" in {
      forAll { (lich: Lich, rogue: Rogue) =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

//          lich.spellCastingModifier

          val blessedLich = lich
            .withSpellsKnown(FireBolt)
            .withCondition(BlessCondition())
            .withIntelligence(1)
            .asInstanceOf[Lich]

          val acThirteenRogue = rogue
            .withNoArmour()
            .withDexterity(16)

          val attackResult = Spell.spellAttack(blessedLich, acThirteenRogue)

          attackResult shouldBe Hit
        }
      }
    }

    "add 1d4 to a saving throw roll" in {
      forAll { paladin: Paladin =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

//          paladin.savingThrowProficiencies

          val blessedPaladin = paladin
            .withProficiencyBonus(1)
            .withCondition(BlessCondition())
            .withStrength(10)

          val (passed, _) =
            SavingThrow.savingThrowPassed(12, Strength, blessedPaladin)

          passed shouldBe true
        }
      }
    }

    "add 1d4 to a Monsters saving throw roll" in {
      forAll { lich: Lich =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

//          lich.savingThrowScores

          val blessedLich = lich
              .withLegendaryResistanceCount(0)
            .withCondition(BlessCondition())

          val (passed, _) =
            SavingThrow.savingThrowPassed(21, Constitution, blessedLich)

          passed shouldBe true
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
