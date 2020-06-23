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

    "apply buff condition to targets including the caster if not enough targets" in {
      forAll { (paladin: Paladin, fighter: Fighter, rogue: Rogue) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val multiTargetBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), numTargets = 3)

          val buffingPaladin = paladin

          val fighterCombatant = fighter.withCombatIndex(2)
          val rogueCombatant = rogue.withCombatIndex(3)

          val (updatedPaladin: Paladin,
            List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedRogue: Rogue))) =
          multiTargetBuffSpell.effect(buffingPaladin, multiTargetBuffSpell.spellLevel,
                List(fighterCombatant, rogueCombatant))

          updatedPaladin.conditions shouldBe List(BlessCondition())
          updatedFighter.conditions shouldBe List(BlessCondition())
          updatedRogue.conditions   shouldBe List(BlessCondition())
        }
      }
    }

    "apply the buff condition to the caster if only of no allies are available" in {
      forAll { paladin: Paladin =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val multiTargetBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

          val buffingPaladin = paladin

          val (updatedPaladin: Paladin, _) =
            multiTargetBuffSpell.effect(buffingPaladin, multiTargetBuffSpell.spellLevel, List.empty[Combatant])

          updatedPaladin.conditions shouldBe List(BlessCondition())
        }
      }
    }
  }

  "focusTanksCreatureOrder" should {
    "have all classes listed in its priority list" in {
      fail("TODO")
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
