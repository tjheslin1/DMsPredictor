package unit.spellcasting

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import util.TestData._

class SingleTargetHealingSpellSpec extends UnitSpecBase {

    "effect" should {
      "heal the target" in {
        forAll { (cleric: Cleric, fighter: Fighter) =>
          new TestContext {
            implicit override val roll: RollStrategy = _ => RollResult(10)

            val fireSpellCleric = cleric
              .withSpellKnown(healingSpell)
              .withAllSpellSlotsAvailable()
              .withChannelDivinityUsed()
              .withWisdom(15)
              .asInstanceOf[Cleric]

            val damagedFighter = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)

            val (_, List(Combatant(_, healedFighter: Fighter))) =
              healingSpell.effect(fireSpellCleric, List(damagedFighter))

            healingSpellUsedCount shouldBe 1
            healedFighter.health shouldBe damagedFighter.creature.health + 4
          }
        }
      }

      "heal the target up to their max health" in {
        forAll { (cleric: Cleric, fighter: Fighter) =>
          new TestContext {
            implicit override val roll: RollStrategy = _ => RollResult(10)

            val fireSpellCleric = cleric
              .withSpellKnown(healingSpell)
              .withAllSpellSlotsAvailable()
              .withChannelDivinityUsed()
              .withWisdom(15)
              .asInstanceOf[Cleric]

            val damagedFighter = fighter.withHealth(98).withMaxHealth(100).withCombatIndex(2)

            val (_, List(Combatant(_, healedFighter: Fighter))) =
              healingSpell.effect(fireSpellCleric, List(damagedFighter))

            healingSpellUsedCount shouldBe 1
            healedFighter.health shouldBe 100
          }
        }
      }
    }

    abstract private class TestContext {
      implicit val roll: RollStrategy

      var healingSpellUsedCount = 0
      val healingSpell = new SingleTargetHealingSpell() {

        val name: String             = "tracked-healing-spell"
        val school: SchoolOfMagic    = Evocation
        val castingTime: CastingTime = OneAction
        val spellLevel: SpellLevel   = 1
        val concentration: Boolean   = false

        def healing[_: RS](spellCaster: SpellCaster): Int = {
          healingSpellUsedCount += 1
          4
        }
      }
    }
  }
