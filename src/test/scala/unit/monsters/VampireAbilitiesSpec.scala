package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Grappled
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.vampire.VampireAbilities._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class VampireAbilitiesSpec extends UnitSpecBase {

  "bite" should {
    "target a creature who is grappled" in {
      forAll { (vampire: Vampire, fighter: Fighter, cleric: Cleric) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val vampireCombatant = vampire.withStrength(20).withCombatIndex(1)
          val fighterCombatant = fighter.withStrength(1).withDexterity(1).withCombatIndex(2)
          val clericCombatant =
            cleric.withCondition(Grappled(18)).withStrength(1).withDexterity(1).withCombatIndex(3)

          val (Combatant(_, updatedVampire: Vampire),
               List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedCleric: Cleric))) =
            bite(1)(vampireCombatant)
              .useAbility(List(fighterCombatant, clericCombatant), LowestFirst)

        }
      }
    }

    "restore the Vampires health equal to the necrotic damage dealt" in {
      fail("todo")
    }

    "reduce the creatures maxHealth (and health accordingly) equal to the necrotic damage taken" in {
      fail("todo")
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
