package unit.cleric

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.castSingleTargetHealingSpell
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.cleric.LifeClericAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.MagicMissile
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class LifeClericAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "discipleOfLife" should {
    "delegate to a healing spell" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val lifeCleric = cleric
            .withSpellKnown(trackedHealingSpell())
            .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
            .withCombatIndex(1)

          val weakFighter = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)

          discipleOfLife(1)(lifeCleric).useAbility(List(weakFighter), LowestFirst)

          trackedHealingSpellUsed shouldBe true
        }
      }
    }

    "restore additional hit points equal to 2 + the healing spells level" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val lifeCleric = cleric
            .withSpellKnown(trackedHealingSpell())
            .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
            .withCombatIndex(1)

          val weakFighter = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            discipleOfLife(1)(lifeCleric).useAbility(List(weakFighter), LowestFirst)

          val trackedSpellHealing = 1
          val expectedHealth = weakFighter.creature.health + trackedSpellHealing +
            discipleOfLifeBonusHealing(trackedHealingSpell().spellLevel)

          updatedFighter.health shouldBe expectedHealth
        }
      }
    }

    "be triggered when a healing spell is triggered (players health is below 50%)" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val lifeCleric = random[Cleric]
        .withSpellKnown(trackedHealingSpell())
        .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
        .withCombatIndex(1)

      val weakFighter = random[Fighter].withHealth(10).withMaxHealth(100).withCombatIndex(2)

      discipleOfLife(1)(lifeCleric).triggerMet(List(weakFighter)) shouldBe true
    }

    "not be triggered when a healing spell is not triggered (players health is below 50%)" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val lifeCleric = random[Cleric]
        .withSpellKnown(trackedHealingSpell())
        .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
        .withCombatIndex(1)

      val weakFighter = random[Fighter].withHealth(90).withMaxHealth(100).withCombatIndex(2)

      discipleOfLife(1)(lifeCleric).triggerMet(List(weakFighter)) shouldBe false
    }

    "not meet the condition if the Spell Caster has only a damage spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withSpellKnown(MagicMissile).withCombatIndex(1)

      discipleOfLife(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withNoSpellSlotsAvailable().withCombatIndex(1)

      discipleOfLife(Priority)(cleric).conditionMet shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var trackedHealingSpellUsed = false
    def trackedHealingSpell() = new Spell {
      val name                               = "tracked-healing-spell"
      val school: SchoolOfMagic              = Evocation
      val castingTime: CastingTime           = OneAction
      val spellEffect: SpellEffect           = HealingSpell
      val spellTargetStyle: SpellTargetStyle = MeleeSpellAttack
      val damageType: DamageType             = Radiant
      val spellLevel: SpellLevel             = 1

      def effect[_: RS](spellCaster: SpellCaster): Int = {
        trackedHealingSpellUsed = true
        1
      }
    }
  }
}
