package unit.spellcasting

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class SingleTargetConditionSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply condition if saving throw failed" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val fireSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val monster = goblin
            .withDexterity(2)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: Goblin))) =
            dexterityConditionSpell.effect(fireSpellCleric, dexterityConditionSpell.spellLevel, List(monster))

          dexteritySaveConditionCount shouldBe 1
          updatedMonster.conditions shouldBe monster.creature.conditions ++ List(dexterityConditionSpell.conditionFrom(fireSpellCleric))
        }
      }
    }

    "not apply condition if saving throw passed" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val fireSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(2)
            .asInstanceOf[Cleric]

          val monster = goblin
            .withDexterity(20)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: Goblin))) =
            dexterityConditionSpell.effect(fireSpellCleric, dexterityConditionSpell.spellLevel, List(monster))

          dexteritySaveConditionCount shouldBe 0
          updatedMonster.conditions shouldBe monster.creature.conditions
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var dexteritySaveConditionCount = 0
    val dexterityConditionSpell = new SingleTargetConditionSpell() {
      val attribute: Attribute = Dexterity

      val name: String             = "tracked-dexterity-save-spell"
      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneAction
      val spellLevel: SpellLevel   = 1
      val requiresConcentration: Boolean   = false

      def conditionFrom(spellCaster: SpellCaster): Condition = Turned(10, 10)

      override def applyCondition[_: RS](spellCaster: SpellCaster, target: Combatant): Combatant = {
        dexteritySaveConditionCount += 1
        super.applyCondition(spellCaster, target)(roll)
      }
    }
  }
}
