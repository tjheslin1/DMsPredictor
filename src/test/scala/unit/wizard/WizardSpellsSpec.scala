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

class WizardSpellsSpec extends UnitSpecBase {

  "acid arrow" should {

    "apply acid arrow condition on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(18, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit override val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.conditions shouldBe List(AcidArrowCondition())
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
          implicit override val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(wizard, 2, List(goblinCombatant))

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

          implicit override val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(wizard, 2, List(goblinCombatant))

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

          implicit override val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 46
        }
      }
    }

    "deal no damage on critical miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(0)

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 50
        }
      }
    }
  }

  "shield spell" should {
    "use a first level spell slot if available" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val ac13Wizard = wizard.withDexterity(10).asInstanceOf[Wizard]

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
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val noSpellSlotsWizard =
            wizard.withMageArmourPrepared(false).withNoSpellSlotsAvailable().withDexterity(10)

          ShieldSpell.updateAttackOnReaction(noSpellSlotsWizard, 15) shouldBe (Hit, noSpellSlotsWizard)
        }
      }
    }

    "return the original attack result if the attack missed" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          ShieldSpell.updateAttackOnReaction(wizard, 0) shouldBe (Miss, wizard)
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
