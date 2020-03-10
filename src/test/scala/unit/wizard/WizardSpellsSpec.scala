package unit.wizard

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.AcidArrowCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.FirstLevelSpellSlots
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._
import util.TestMonster

class WizardSpellsSpec extends UnitSpecBase {

  "acid arrow" should {

    "apply acid arrow condition on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(18, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.conditions shouldBe List(AcidArrowCondition(2))
        }
      }
    }

    "deal damage twice on critical hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(20, // attack roll
            2, 2, 2, 2, 2, 2, 2, 2) // damage rolls (4d4 twice)
          // format: n
          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 34
        }
      }
    }

    "deal damage on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(18, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 42
        }
      }
    }

    "deal half damage on miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(2, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 46
        }
      }
    }

    "deal no damage on critical miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(0)

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 50
        }
      }
    }

    "be cast using the highest spell slot available" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(19, // attack roll
            2, 2, 2, 2, 2) // damage rolls (5d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFiveWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val fiftyHpGoblin = goblin
            .withArmourClass(1)
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFiveWizard, 3, List(fiftyHpGoblin))

          updatedGoblin.health shouldBe 40
        }
      }
    }
  }

  "shield spell" should {
    "use a first level spell slot if available" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val ac13Wizard =
            wizard.withMageArmourPrepared(true).withDexterity(10).asInstanceOf[Wizard]

          val (_, updatedSpellcaster: SpellCaster) =
            ShieldSpell.updateAttackOnReaction(ac13Wizard, 15)

          val expectedSpellSlots = ac13Wizard.spellSlots.copy(
            firstLevel = FirstLevelSpellSlots(ac13Wizard.spellSlots.firstLevel.count - 1))

          updatedSpellcaster.spellSlots shouldBe expectedSpellSlots
        }
      }
    }

    "return the original attack result if no first level spell slots are available" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val noSpellSlotsWizard =
            wizard.withMageArmourPrepared(false).withNoSpellSlotsAvailable().withDexterity(10)

          ShieldSpell.updateAttackOnReaction(noSpellSlotsWizard, 15) shouldBe (Hit, noSpellSlotsWizard)
        }
      }
    }

    "return the original attack result if the attack missed" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          ShieldSpell.updateAttackOnReaction(wizard, 0) shouldBe (Miss, wizard)
        }
      }
    }

    "no longer be in effect at the start of the casters next turn" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val ac13Wizard =
            wizard.withMageArmourPrepared(true).withDexterity(10).asInstanceOf[Wizard]

          fail("TODO: test shield buff is removed")
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
