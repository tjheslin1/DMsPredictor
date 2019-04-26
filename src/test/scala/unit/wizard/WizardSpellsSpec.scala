package unit.wizard

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.AcidArrowCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.AcidArrow
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class WizardSpellsSpec extends UnitSpecBase {

  "acid arrow" should {

    "apply acid arrow condition on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          val diceRolls = Iterator(18, // attack roll
            2, 2, 2, 2 // damage rolls (4d4)
          )
          override implicit val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.conditions shouldBe List(AcidArrowCondition())
        }
      }
    }

    "deal damage twice on critical hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          val diceRolls = Iterator(20, // attack roll
            2, 2, 2, 2, 2, 2, 2, 2 // damage rolls (4d4 twice)
          )
          override implicit val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 34
        }
      }
    }

    "deal damage on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          val diceRolls = Iterator(18, // attack roll
            2, 2, 2, 2 // damage rolls (4d4)
          )
          override implicit val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 42
        }
      }
    }

    "deal half damage on miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          val diceRolls = Iterator(2, // attack roll
            2, 2, 2, 2 // damage rolls (4d4)
          )
          override implicit val roll: RollStrategy = _ => RollResult(diceRolls.next())

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 46
        }
      }
    }

    "deal no damage on critical miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(0)

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) = AcidArrow.effect(wizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 50
        }
      }
    }
  }

  private abstract class TestContext {
    implicit val roll: RollStrategy
  }
}
