package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import util.TestData._

class MultiTargetBuffSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply buff condition to targets" in {
      forAll { (paladin: Paladin, fighter: Fighter, rogue: Rogue) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val multiTargetBuffSpell = trackedMultiTargetBuffSpell(1, numTargets = 3)

          val buffingPaladin = paladin

          val fighterCombatant = fighter.withCombatIndex(2)
          val rogueCombatant = rogue.withCombatIndex(2)

          val (updatedPaladin: Paladin,
            List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedRogue: Rogue))) =
              multiTargetBuffSpell.effect(buffingPaladin, multiTargetBuffSpell.spellLevel,
                List(fighterCombatant, rogueCombatant))

          updatedPaladin.conditions shouldBe List(BlessCondition)
          updatedFighter.conditions shouldBe List(BlessCondition)
          updatedRogue.conditions   shouldBe List(BlessCondition)
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
