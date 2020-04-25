package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class SingleTargetInstantEffectSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply instant effect to the target" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, setHealthToOneEffect)

          val instantEffectCastingWizard = wizard
            .withSpellKnown(trackedInstantSpell)

          val goblinCombatant = goblin
              .withHealth(50)
              .withMaxHealth(50)
              .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            trackedInstantSpell.effect(wizard, trackedInstantSpell.spellLevel, List(goblinCombatant))

          updatedGoblin.health shouldBe 1
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
