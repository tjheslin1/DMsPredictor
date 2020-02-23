package unit.spellcasting

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class ConcentrationConditionSpellSpec extends UnitSpecBase {

  "effect" should {
    "apply condition to first enemy if single target spell and saving throw failed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = dexterityConditionSaveSpell(singleTargetSpell = true)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
          List(Combatant(_, updatedGoblinOne: Goblin),
          Combatant(_, updatedGoblinTwo: Goblin),
          Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
              dexterityConditionSpell.spellLevel,
              List(lowDexGoblin(goblinOne, 2),
                lowDexGoblin(goblinTwo, 3),
                lowDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

          dexteritySaveConditionCount shouldBe 1

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions
        }
      }
    }

    "apply condition to all enemies if saving throw failed" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin, goblinThree: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = dexterityConditionSaveSpell(singleTargetSpell = false)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                lowDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

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
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = dexterityConditionSaveSpell(singleTargetSpell = false)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val (_,
               List(Combatant(_, updatedGoblinOne: Goblin),
                    Combatant(_, updatedGoblinTwo: Goblin),
                    Combatant(_, updatedGoblinThree: Goblin))) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(lowDexGoblin(goblinOne, 2),
                                                lowDexGoblin(goblinTwo, 3),
                                                highDexGoblin(goblinThree, 4)))

          val expectedCondition = List(dexterityConditionSpell.conditionFrom(conditionSpellCleric))

          dexteritySaveConditionCount shouldBe 2

          updatedGoblinOne.conditions shouldBe goblinOne.creature.conditions ++ expectedCondition
          updatedGoblinTwo.conditions shouldBe goblinTwo.creature.conditions ++ expectedCondition
          updatedGoblinThree.conditions shouldBe goblinThree.creature.conditions
        }
      }
    }

    "set cleric to concentrating on spell if at least one enemy failed the saving throw" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = dexterityConditionSaveSpell(singleTargetSpell = false)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(15)
            .asInstanceOf[Cleric]

          val slowGoblin = lowDexGoblin(goblinOne, 2)
          val quickGoblin = highDexGoblin(goblinTwo, 3)

          val (updatedCleric: Cleric, _) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(slowGoblin, quickGoblin))

          updatedCleric.isConcentrating shouldBe true
          updatedCleric.concentratingSpell shouldBe dexterityConditionSpell.some
        }
      }
    }

    "not set cleric to concentrating on spell if at least one enemy failed the saving throw" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val dexterityConditionSpell = dexterityConditionSaveSpell(singleTargetSpell = false)

          val conditionSpellCleric = cleric
            .withSpellKnown(dexterityConditionSpell)
            .withAllSpellSlotsAvailableForLevel(cleric.level)
            .withChannelDivinityUsed()
            .withWisdom(5)
            .asInstanceOf[Cleric]

          val slowGoblin = highDexGoblin(goblinOne, 2)
          val quickGoblin = highDexGoblin(goblinTwo, 3)

          val (updatedCleric: Cleric, _) =
            dexterityConditionSpell.effect(conditionSpellCleric,
                                           dexterityConditionSpell.spellLevel,
                                           List(slowGoblin, quickGoblin))

          updatedCleric.isConcentrating shouldBe false
          updatedCleric.concentratingSpell shouldBe none[Spell]
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
    def dexterityConditionSaveSpell(singleTargetSpell: Boolean): ConcentrationConditionSpell = new ConcentrationConditionSpell() {
      val name: String                   = "tracked-multi-dexterity-save-spell"

      val attribute: Attribute = Dexterity
       val singleTarget: Boolean = singleTargetSpell

      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneActionCast
      val spellLevel: SpellLevel         = 1

      def conditionFrom(spellCaster: SpellCaster): Condition = Turned(10, 10)

      override def applyCondition[_: RS](spellCaster: SpellCaster, target: Combatant): Combatant = {
        dexteritySaveConditionCount += 1
        super.applyCondition(spellCaster, target)(roll)
      }
}
  }
}
