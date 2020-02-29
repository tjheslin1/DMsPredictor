package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellOfTypeBelowLevel" should {
    "return a spell of a specific SpellEffect equal to the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 1)().get shouldBe (GuidingBolt, GuidingBolt.spellLevel)
    }

    "return a multi attack spell of a specific SpellEffect equal to the level given" in {
      val wizard = random[Wizard].withSpellsKnown(FireBolt, MagicMissile, AcidArrow, Fireball)

      spellOfLevelOrBelow(wizard, DamageSpell, 3)(multiAttackOnly = true).get shouldBe (Fireball, Fireball.spellLevel)
    }

    "return a spell of a specific SpellEffect below the level given using the lowest spell slot" in new Tracking {
      val trackedLevelOneDamageSpell = trackedMeleeSpellAttack(1, higherSpellSlot = false)

      val cleric = random[Cleric]
        .withSpellsKnown(SacredFlame, trackedLevelOneDamageSpell, CureWounds, HoldPerson)
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withLevel(LevelFive)
        .asInstanceOf[Cleric]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(cleric, DamageSpell, 3)().get

      actualSpell.name shouldBe trackedLevelOneDamageSpell.name
      actualSpellLevel.value shouldBe 1
    }

    "return a spell of a specific SpellEffect below the level given using the highest spell slot" in new Tracking {
      val trackedLevelOneDamageSpell = trackedMeleeSpellAttack(1, higherSpellSlot = true)

      val cleric = random[Cleric]
        .withSpellsKnown(SacredFlame, trackedLevelOneDamageSpell, CureWounds, HoldPerson)
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withLevel(LevelFive)
        .asInstanceOf[Cleric]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(cleric, DamageSpell, 3)().get

      actualSpell.name shouldBe trackedLevelOneDamageSpell.name
      actualSpellLevel.value shouldBe 3
    }

    "return a multi attack spell of a specific SpellEffect below the level given using the lowest spell slot" in new Tracking {
      val trackedMultiAttackDamageSpell =
        trackedMultiMeleeSpellAttack(1, concentration = false, higherSpellSlot = false)

      val wizard = random[Wizard].withSpellsKnown(FireBolt,
                                                  MagicMissile,
                                                  trackedMultiAttackDamageSpell,
                                                  AcidArrow)

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(wizard, DamageSpell, 2)(multiAttackOnly = true).get

      actualSpell.name shouldBe trackedMultiAttackDamageSpell.name
      actualSpellLevel.value shouldBe 1
    }

    "return a multi attack spell of a specific SpellEffect below the level given using the highest spell slot" in new Tracking {
      val trackedLevelTwoMultiSpell =
        trackedMultiMeleeSpellAttack(2, concentration = false, higherSpellSlot = true)

      val cleric = random[Cleric].withSpellsKnown(FireBolt,
                                                  MagicMissile,
                                                  trackedLevelTwoMultiSpell,
                                                  trackedSingleTargetSavingThrowSpell(3, Wisdom))

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(cleric, DamageSpell, 3)(multiAttackOnly = true).get

      actualSpell.name shouldBe trackedLevelTwoMultiSpell.name
      actualSpellLevel.value shouldBe 3
    }

    "not return a concentration spell if already concentrating when checkConcentration is set to false" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentratingOn(trackedConditionSpell(1))
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3)(checkConcentration = false) shouldBe none[(Spell, SpellLevel)]
    }

    "not return a concentration spell if already concentrating when checkConcentration is set to true" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentratingOn(trackedConditionSpell(1))
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3)() shouldBe none[(Spell, SpellLevel)]
    }

    "return the highest spell slot for a spell which benefits from a higher slot" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val spellToCastHighestLevel = trackedSelfBuffSpell(HuntersMarkBuffCondition,
                                                         spellLvl = 1,
                                                         concentration = false,
                                                         higherSpellSlot = true)

      val clericWithLevelThirdLevelSpellSlots = random[Cleric]
        .withSpellKnown(spellToCastHighestLevel)
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withLevel(LevelFive)
        .asInstanceOf[Cleric]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(clericWithLevelThirdLevelSpellSlots, BuffSpell, 3)().get

      actualSpell.name shouldBe spellToCastHighestLevel.name
      actualSpellLevel.value shouldBe 3
    }

    "return a the lowest spell slot for a spell which does not benefit from a higher slot" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val spellToCastLowestLevel = trackedSelfBuffSpell(HuntersMarkBuffCondition,
                                                        spellLvl = 1,
                                                        concentration = true,
                                                        higherSpellSlot = false)

      val ranger = random[Ranger]
        .withSpellKnown(spellToCastLowestLevel)
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withLevel(LevelFive)
        .asInstanceOf[Ranger]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(ranger, BuffSpell, 2)().get

      actualSpell.name shouldBe spellToCastLowestLevel.name
      actualSpellLevel.value shouldBe 1
    }

    "not return a higher spell slot for a cantrip" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      // expecting `higherSpellSlot to be ignored for a cantrip
      val trackedCantrip = trackedSingleTargetSavingThrowSpell(0, Strength, higherSpellSlot = true)

      val cleric = random[Cleric]
        .withSpellsKnown(trackedCantrip, CureWounds, HoldPerson)
        .withAllSpellSlotsAvailableForLevel(LevelThree)
        .withLevel(LevelThree)
        .asInstanceOf[Cleric]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(cleric, DamageSpell, 2)().get

      actualSpell.name shouldBe trackedCantrip.name
      actualSpellLevel.value shouldBe 0
    }

    "return none if no spell of SpellEffect is found" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds)

      spellOfLevelOrBelow(cleric, ConcentrationSpell, 2)() shouldBe none[(Spell, SpellLevel)]
    }
  }

  "spellSavingThrowPassed" should {
    "return true if the targets roll equals the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe true
        }
      }
    }

    "return true if the targets roll exceeds the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(14)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe true
        }
      }
    }

    "return false if the targets roll is less than the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(14).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          spellSavingThrowPassed(caster, Dexterity, monster) shouldBe false
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
