package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class SingleTargetHealingSpellSpec extends UnitSpecBase {

  "effect" should {
    "heal the target" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val healingSpell = trackedHealingSpell(1, healingDone = 4)

          val healingCleric = cleric
            .withSpellKnown(healingSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val damagedFighter = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) = healingSpell.effect(
            healingCleric,
            healingSpell.spellLevel,
            List(damagedFighter))

          trackedHealingSpellUsedCount shouldBe 1
          healedFighter.health shouldBe damagedFighter.creature.health + 4
        }
      }
    }

    "heal the target up to their max health" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val healingSpell = trackedHealingSpell(1, healingDone = 4)

          val healingCleric = cleric
            .withSpellKnown(healingSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val damagedFighter = fighter.withHealth(98).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) = healingSpell.effect(
            healingCleric,
            healingSpell.spellLevel,
            List(damagedFighter))

          trackedHealingSpellUsedCount shouldBe 1
          healedFighter.health shouldBe 100
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
