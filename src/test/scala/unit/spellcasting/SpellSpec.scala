package unit.spellcasting

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells.HuntersMarkBuffCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import util.TestData._
import util.TestMonster

class SpellSpec extends UnitSpecBase {

  "spellOfLevelOrBelow" should {
    "return a spell of a specific SpellEffect equal to the level given" in {
      val cleric = random[Cleric].withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(cleric, DamageSpell, 1)().get shouldBe (GuidingBolt, GuidingBolt.spellLevel)
    }

    "return a single target attack spell of a specific SpellEffect equal to the level given" in {
      val paladin = random[Paladin].withSpellsKnown(SacredFlame, Fireball)

      spellOfLevelOrBelow(paladin, DamageSpell, 3)(singleTargetSpellsOnly = true).get shouldBe (SacredFlame, SacredFlame.spellLevel)
    }

    "return a single target attack spell when looking down the spell list" in new Tracking {
      val trackedThirdLevelSingleTargetSpell = trackedSingleTargetSavingThrowSpell(3, Dexterity, higherSpellSlot = true)
      val trackedSixthLevelMultiTargetSpell = trackedMultiTargetSavingThrowSpell(6, Dexterity, higherSpellSlot = true)

      val lich = random[Lich].withSpellsKnown(
        trackedThirdLevelSingleTargetSpell,
        trackedSixthLevelMultiTargetSpell
      )

      val (foundSpell, foundSpellLevel) = spellOfLevelOrBelow(lich, DamageSpell, 9)(singleTargetSpellsOnly = true).get

      foundSpell.name shouldBe trackedThirdLevelSingleTargetSpell.name
      foundSpellLevel.value shouldBe 9
    }

    "return a multi target spell of a specific SpellEffect equal to the level given" in {
      val wizard = random[Wizard].withSpellsKnown(FireBolt, MagicMissile, AcidArrow, Fireball)

      spellOfLevelOrBelow(wizard, DamageSpell, 3)(multiTargetSpellsOnly = true).get shouldBe (Fireball, Fireball.spellLevel)
    }

    "return a multi target spell when looking down the spell list" in new Tracking {
      val trackedThirdLevelMultiTargetSpell = trackedMultiTargetSavingThrowSpell(3, Dexterity, higherSpellSlot = true)
      val trackedSixthLevelSingleTargetSpell = trackedSingleTargetSavingThrowSpell(6, Dexterity, higherSpellSlot = true)

      val lich = random[Lich].withSpellsKnown(
        trackedThirdLevelMultiTargetSpell,
        trackedSixthLevelSingleTargetSpell
      )

      val (foundSpell, foundSpellLevel) = spellOfLevelOrBelow(lich, DamageSpell, 9)(multiTargetSpellsOnly = true).get

      foundSpell.name shouldBe trackedThirdLevelMultiTargetSpell.name
      foundSpellLevel.value shouldBe 9
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

    "return a multi target spell of a specific SpellEffect below the level given using the lowest spell slot" in new Tracking {
      val trackedMultiAttackDamageSpell =
        trackedMultiMeleeSpellAttack(1, concentration = false, higherSpellSlot = false)

      val wizard = random[Wizard].withSpellsKnown(FireBolt,
                                                  MagicMissile,
                                                  trackedMultiAttackDamageSpell,
                                                  AcidArrow)

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(wizard, DamageSpell, 2)(multiTargetSpellsOnly = true).get

      actualSpell.name shouldBe trackedMultiAttackDamageSpell.name
      actualSpellLevel.value shouldBe 1
    }

    "return a multi target spell of a specific SpellEffect below the level given using the highest spell slot" in new Tracking {
      val trackedLevelTwoMultiTargetSpell =
        trackedMultiMeleeSpellAttack(2, concentration = false, higherSpellSlot = true)

      val cleric = random[Cleric].withSpellsKnown(FireBolt,
                                                  MagicMissile,
                                                  trackedLevelTwoMultiTargetSpell,
                                                  trackedSingleTargetSavingThrowSpell(3, Wisdom))

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(cleric, DamageSpell, 3)(multiTargetSpellsOnly = true).get

      actualSpell.name shouldBe trackedLevelTwoMultiTargetSpell.name
      actualSpellLevel.value shouldBe 3
    }

    "return a concentration spell if already concentrating when checkCasterIsConcentrating is set to false" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentratingOn(HoldPerson)
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      val (actualSpell, actualSpellLevel) =
        spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3)(checkCasterIsConcentrating = false).get

      actualSpell shouldBe HoldPerson
      actualSpellLevel shouldBe HoldPerson.spellLevel
    }

    "not return a concentration spell if already concentrating when checkCasterIsConcentrating is set to true" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val concentratingCleric = random[Cleric]
        .withConcentratingOn(HoldPerson)
        .withSpellsKnown(SacredFlame, GuidingBolt, CureWounds, HoldPerson)

      spellOfLevelOrBelow(concentratingCleric, ConcentrationSpell, 3)(checkCasterIsConcentrating = true) shouldBe none[(Spell, SpellLevel)]
    }

    "return the highest spell slot for a spell which benefits from a higher slot" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val spellToCastAtHigherLevel = trackedSelfBuffSpell(HuntersMarkBuffCondition,
                                                         spellLvl = 1,
                                                         concentration = false,
                                                         higherSpellSlot = true)

      val clericWithLevelThirdLevelSpellSlots = random[Cleric]
        .withSpellKnown(spellToCastAtHigherLevel)
        .withAllSpellSlotsAvailableForLevel(LevelFive)
        .withLevel(LevelFive)
        .asInstanceOf[Cleric]

      val (actualSpell, actualSpellLevel) = spellOfLevelOrBelow(clericWithLevelThirdLevelSpellSlots, BuffSpell, 3)().get

      actualSpell.name shouldBe spellToCastAtHigherLevel.name
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

      // expecting `higherSpellSlot` to be ignored for a cantrip
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

  "spellAttackBonus" should {
    "combine the Players spellcasting modifier and their attribute modifier" in {
      forAll { wizard: Wizard =>
        spellAttackBonus(wizard) shouldBe wizard.spellCastingModifier + mod(wizard.stats.intelligence)
      }
    }

    "combine the Monster (SpellCaster) spellcasting modifier and their attribute modifier" in {
      forAll { lich: Lich =>
        spellAttackBonus(lich) shouldBe lich.spellCastingModifier + mod(lich.stats.intelligence)
      }
    }
  }

  "spellSaveDc" should {
    "combine the Players spellcasting modifier and their attribute modifier" in {
      forAll { wizard: Wizard =>
        spellSaveDc(wizard) shouldBe 8 + wizard.spellCastingModifier + mod(wizard.stats.intelligence)
      }
    }

    "combine the Monster (SpellCaster) spellcasting modifier and their attribute modifier" in {
      forAll { lich: Lich =>
        spellSaveDc(lich) shouldBe 8 + lich.spellCastingModifier + mod(lich.stats.intelligence)
      }
    }
  }

  "spellSavingThrowPassed" should {
    "return true if the targets roll equals the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          val (passed, _) = spellSavingThrowPassed(caster, Dexterity, monster)

          passed shouldBe true
        }
      }
    }

    "return true if the targets roll exceeds the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(10).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(14)

          val (passed, _) = spellSavingThrowPassed(caster, Dexterity, monster)

          passed shouldBe true
        }
      }
    }

    "return false if the targets roll is less than the caster's spell save DC" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val caster  = cleric.withProficiencyBonus(2).withWisdom(14).asInstanceOf[Cleric]
          val monster = testMonster.withDexterity(10)

          val (passed, _) = spellSavingThrowPassed(caster, Dexterity, monster)

          passed shouldBe false
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val rollStrategy: RollStrategy
  }
}
