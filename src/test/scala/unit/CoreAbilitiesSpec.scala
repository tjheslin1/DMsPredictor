package unit

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.MagicMissile
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

class CoreAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Extra Attack" should {

    "make two weapon attacks" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val swordedFighter = fighter
            .withBaseWeapon(trackedSword)
            .withAbilities(List(extraAttack(Priority)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          extraAttack(Priority)(swordedFighter)
            .useAbility(List(monster), LowestFirst)

          swordUsedCount shouldBe 2
        }
      }
    }

    "delegate to an ability lower in the order which can be used during an Attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withAbilities(
              List(extraAttack(Priority),
                   trackedAbility(2, action = WholeAction),
                   otherTrackedAbility(3, action = SingleAttack, updatedTracking = ())))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          extraAttack(Priority)(trackedAbilityFighter).useAbility(List(monster), LowestFirst)

          otherTrackedAbilityUsedCount shouldBe 2
          trackedAbilityUsedCount shouldBe 0
        }
      }
    }

    "delegate to an ability lower in order then default to an attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withAbilities(
              List(extraAttack(Priority),
                   trackedAbility(2, action = WholeAction),
                otherTrackedAbility(3, action = SingleAttack)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          extraAttack(Priority)(trackedAbilityFighter).useAbility(List(monster), LowestFirst)

          otherTrackedAbilityUsedCount shouldBe 1
          trackedAbilityUsedCount shouldBe 0
        }
      }
    }
  }

  "castSingleTargetOffensiveSpell" should {

    "cast a spell (spell attack)" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedMeleeSpellAttack(2))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedCleric = cleric
            .withSpellKnown(trackedSingleTargetSavingThrowSpell(2, Wisdom))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(10)
            .withCombatIndex(1)

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          singleSavingThrowSpellUsedCount shouldBe 1
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot which has a damaging spell" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedCleric = cleric
            .withSpellKnown(trackedSingleTargetSavingThrowSpell(2, Wisdom))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(10)
            .asInstanceOf[Cleric]

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(trackedCleric.withCombatIndex(1))
              .useAbility(List(monster), LowestFirst)

          singleSavingThrowSpellUsedCount shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedMeleeSpellAttack(1))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedCleric: Cleric =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withCantrip(trackedSingleTargetSavingThrowSpell(0, Wisdom))
            .withSpellsKnown(trackedSingleTargetSavingThrowSpell(0, Wisdom), trackedHealingSpell(3))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedCleric =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "cast cantrip if defined and no spell slots are available" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val noSpellSlotsCleric = cleric
            .withCantrip(trackedMeleeSpellAttack(0))
            .withSpellKnown(trackedSingleTargetSavingThrowSpell(1, Wisdom))
            .withNoSpellSlotsAvailable()
            .withWisdom(24)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(2).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(noSpellSlotsCleric)
            .useAbility(List(monster), LowestFirst)

          singleSavingThrowSpellUsedCount shouldBe 0
          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "not meet the condition if the Spell Caster has no damaging spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withSpellKnown(CureWounds).withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoCantrip()
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }
  }

  "castSingleTargetHealingSpell" should {

    "trigger when a players health is below 50%" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric    = random[Cleric].withWisdom(12).withCombatIndex(1)
      val damagedFighter   = random[Fighter].withHealth(25).withMaxHealth(100).withCombatIndex(2)
      val healthyBarbarian = random[Barbarian].withHealth(100).withMaxHealth(100).withCombatIndex(3)
      val goblin           = random[Goblin].withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(damagedFighter, healthyBarbarian, goblin)) shouldBe true
    }

    "trigger when a players health is 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric  = random[Cleric].withWisdom(12).withCombatIndex(1)
      val damagedFighter = random[Fighter].withHealth(80).withMaxHealth(100).withCombatIndex(2)
      val unconsciousBarbarian =
        random[Barbarian].withHealth(0).withMaxHealth(100).withCombatIndex(3)
      val goblin = random[Goblin].withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(damagedFighter, unconsciousBarbarian, goblin)) shouldBe true
    }

    "not trigger when no players health are below 50%" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val healingCleric    = random[Cleric].withWisdom(12).withCombatIndex(1)
      val healthyFighter   = random[Fighter].withHealth(90).withMaxHealth(100).withCombatIndex(2)
      val healthyBarbarian = random[Barbarian].withHealth(100).withMaxHealth(100).withCombatIndex(3)
      val goblin           = random[Goblin].withHealth(10).withMaxHealth(50).withCombatIndex(4)

      castSingleTargetHealingSpell(Priority)(healingCleric)
        .triggerMet(List(healthyFighter, healthyBarbarian, goblin)) shouldBe false
    }

    "cast a spell (healing) using the highest available spell slot" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellKnown(trackedHealingSpell(3))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withWisdom(12)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val damagedFighter = fighter.withHealth(10).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) =
            castSingleTargetHealingSpell(Priority)(healingCleric)
              .useAbility(List(damagedFighter), LowestFirst)

          trackedHealingSpellUsed shouldBe true
          healedFighter.creature.health shouldBe 11
        }
      }
    }

    "cast a spell (healing) using the highest available spell slot which has a healing spell" in {
      forAll { (cleric: Cleric, fighter: Fighter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellKnown(trackedHealingSpell(2))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withWisdom(12)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val damagedFighter = fighter.withHealth(10).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) =
            castSingleTargetHealingSpell(Priority)(healingCleric)
              .useAbility(List(damagedFighter), LowestFirst)

          trackedHealingSpellUsed shouldBe true
          healedFighter.creature.health shouldBe 11
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(1))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val updatedCleric =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1)).update
              .asInstanceOf[Cleric]

          updatedCleric.spellSlots.thirdLevel.count shouldBe (healingCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withCantrip(trackedHealingSpell(0))
            .withSpellsKnown(trackedHealingSpell(0), trackedMeleeSpellAttack(1))
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val updatedCleric =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1)).update
              .asInstanceOf[Cleric]

          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has no healing spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withSpellKnown(MagicMissile).withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withNoSpellSlotsAvailable().withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }
  }

  "castConditionSpell" should {

    "cast a spell (condition)" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedConditionSpell(2))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster.withCombatIndex(2)

          castConcentrationSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          conditionSpellUsedCount shouldBe 1
        }
      }
    }

    "set the spellCasters concentration to the cast spell if a concentration spell" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(SpiritGuardians)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(20)
            .withCombatIndex(1)

          val monster = goblin.withWisdom(2).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConcentrationSpell(Priority)(trackedCleric)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.concentratingSpell shouldBe SpiritGuardians.some
        }
      }
    }

    "cast a spell (condition) using the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedCleric = cleric
            .withSpellKnown(trackedConditionSpell(2))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withCombatIndex(2)

          castConcentrationSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          conditionSpellUsedCount shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedConditionSpell(1))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedCleric: Cleric =
            castConcentrationSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "not meet the condition if the Spell Caster has no condition spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withSpellKnown(MagicMissile).withCombatIndex(1)

      castConcentrationSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoCantrip()
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castConcentrationSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster is concentrating and has no non-concentration condition spells to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val concentrationConditionSpell = trackedConditionSpell(spellLvl = 2)

      val concentratingCleric = random[Cleric]
        .withSpellsKnown(concentrationConditionSpell, trackedSingleTargetSavingThrowSpell(1, Wisdom))
        .withConcentrating(concentrationConditionSpell.some)
        .withLevel(LevelThree)
        .withCombatIndex(1)

      castConcentrationSpell(Priority)(concentratingCleric).conditionMet shouldBe false
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
