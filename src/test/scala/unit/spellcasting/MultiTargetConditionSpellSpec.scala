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

class MultiTargetConditionSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply condition to all enemies if saving throw failed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val fireSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(fireSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                lowDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(fireSpellCleric))

          dexteritySaveConditionCount shouldBe 3

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions ++ expectedCondition
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions ++ expectedCondition
        }
      }
    }

    "not apply condition to enemies if saving throw passed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val fireSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(fireSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                highDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(fireSpellCleric))

          dexteritySaveConditionCount shouldBe 2

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions ++ expectedCondition
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions
        }
      }
    }
  }

  private def lowDexGoblin(goblin: Goblin, combatIndex: Int) =
    goblin.withDexterity(2).withCombatIndex(combatIndex)

  private def highDexGoblin(goblin: Goblin, combatIndex: Int) =
    goblin.withDexterity(26).withCombatIndex(combatIndex)

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var dexteritySaveConditionCount = 0
    val dexterityConditionSpell = new MultiTargetConditionSpell() {
      val attribute: Attribute = Dexterity

      val name: String             = "tracked-multi-dexterity-save-spell"
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
