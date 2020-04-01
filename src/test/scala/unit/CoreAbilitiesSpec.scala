package unit

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellSlots}
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

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedMeleeSpellAttack(1, higherSpellSlot = false))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedCleric: Cleric =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.firstLevel.count shouldBe (trackedCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
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
            .withSpellsKnown(trackedMeleeSpellAttack(0),
                             trackedSingleTargetSavingThrowSpell(1, Wisdom))
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

      val cleric = random[Cleric].withSpellKnown(CureWounds).withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withSpellsKnown(List.empty[Spell]: _*)
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Single Target Damage spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedMultiMeleeSpellAttack(1), trackedSingleTargetSavingThrowSpell(3, Wisdom))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "not meet the condition if the Spell Caster only knows multi target damaging spells" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedMultiMeleeSpellAttack(1), trackedMultiMeleeSpellAttack(2))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
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
            .withLevel(LevelFive)
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
            .withLevel(LevelFive)
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

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(1, higherSpellSlot = false))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val updatedCleric =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1)).update
              .asInstanceOf[Cleric]

          updatedCleric.spellSlots.firstLevel.count shouldBe (healingCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
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

      val cleric = random[Cleric].withSpellKnown(MagicMissile).withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoSpellSlotsAvailable().withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any healing spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(trackedSingleTargetSavingThrowSpell(2, Wisdom), trackedHealingSpell(3))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetHealingSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
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
      forAll { cleric: Cleric =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedConditionSpell(1))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val updatedCleric: Cleric =
            castConcentrationSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedCleric = cleric
            .withSpellKnown(trackedConditionSpell(1, higherSpellSlot = false))
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val updatedCleric: Cleric =
            castConcentrationSpell(Priority)(clericCombatant).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.firstLevel.count shouldBe (trackedCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has no condition spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(MagicMissile).withCombatIndex(1)

      castConcentrationSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castConcentrationSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster is concentrating and has no non-concentration condition spells to cast" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val concentrationConditionSpell = trackedConditionSpell(spellLvl = 2)

          val concentratingCleric = cleric
            .withSpellsKnown(concentrationConditionSpell,
                             trackedSingleTargetSavingThrowSpell(1, Wisdom))
            .withConcentratingOn(concentrationConditionSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          castConcentrationSpell(Priority)(concentratingCleric).conditionMet shouldBe false
        }
      }
    }
  }

  "castMultiTargetOffensiveSpell" should {
    "cast a spell (spell attack)" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedWizard = wizard
            .withSpellKnown(trackedMultiMeleeSpellAttack(2))
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne.withDexteritySavingThrowScore(-10).withCombatIndex(2)
          val monsterTwo = testMonsterTwo.withDexteritySavingThrowScore(-10).withCombatIndex(3)

          castMultiTargetOffensiveSpell(Priority)(trackedWizard)
            .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          multiMeleeSpellUsedCount shouldBe 2
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedWizard = wizard
            .withSpellKnown(trackedMultiTargetSavingThrowSpell(2, Wisdom))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne.withDexteritySavingThrowScore(-10).withCombatIndex(2)
          val monsterTwo = testMonsterTwo.withDexteritySavingThrowScore(-10).withCombatIndex(3)

          castMultiTargetOffensiveSpell(Priority)(trackedWizard)
            .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          multiSavingThrowSpellUsedCount shouldBe 2
        }
      }
    }

    "cast a multi target spell" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedWizard = wizard
            .withSpellsKnown(trackedMultiMeleeSpellAttack(2),
                             trackedSingleTargetSavingThrowSpell(3, Wisdom))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne.withDexteritySavingThrowScore(-10).withCombatIndex(2)
          val monsterTwo = testMonsterTwo.withDexteritySavingThrowScore(-10).withCombatIndex(3)

          castMultiTargetOffensiveSpell(Priority)(trackedWizard)
            .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          singleSavingThrowSpellUsedCount shouldBe 0
          multiMeleeSpellUsedCount shouldBe 2
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot which has a damaging spell" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedWizard = wizard
            .withSpellKnown(trackedMultiTargetSavingThrowSpell(2, Wisdom))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withIntelligence(20)
            .asInstanceOf[Wizard]

          val monsterOne = testMonsterOne.withDexteritySavingThrowScore(-10).withCombatIndex(2)
          val monsterTwo = testMonsterTwo.withDexteritySavingThrowScore(-10).withCombatIndex(3)

          castMultiTargetOffensiveSpell(Priority)(trackedWizard.withCombatIndex(1))
            .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          multiSavingThrowSpellUsedCount shouldBe 2
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedWizard = wizard
            .withSpellKnown(trackedMultiMeleeSpellAttack(1))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val updatedWizard: Wizard =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant).update.asInstanceOf[Wizard]

          updatedWizard.spellSlots.thirdLevel.count shouldBe (trackedWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedWizard = wizard
            .withSpellKnown(trackedMultiMeleeSpellAttack(1, higherSpellSlot = false))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val updatedWizard: Wizard =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant).update.asInstanceOf[Wizard]

          updatedWizard.spellSlots.firstLevel.count shouldBe (trackedWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.thirdLevel.count shouldBe trackedWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has only a damaging cantrip to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Wizard]
        .withSpellKnown(trackedSingleTargetSavingThrowSpell(0, Wisdom))
        .withSpellsKnown(List.empty[Spell]: _*)
        .withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no damaging spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(CureWounds).withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castMultiTargetOffensiveSpell(Priority)(wizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Multi Attack spells at its level" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val wizardCombatant = wizard
            .withSpellsKnown(MagicMissile, AcidArrow, Fireball)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castMultiTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }
  }

  "castSelfBuffSpell" should {
    "cast a spell (Self Buff) updating the casters conditions" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)

          val buffingRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellKnown(trackedBuffSpell)
            .withLevel(LevelTwo)
            .asInstanceOf[Ranger]

          val rangerCombatant = buffingRanger.withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) =
            castSelfBuffSpell(Priority)(rangerCombatant).useAbility(List.empty[Combatant],
                                                                    LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)

          val buffingRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellKnown(trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Ranger]

          val rangerCombatant = buffingRanger.withCombatIndex(1)

          val updatedRanger = castSelfBuffSpell(Priority)(rangerCombatant).update
            .asInstanceOf[Ranger]

          updatedRanger.spellSlots.secondLevel.count shouldBe buffingRanger.spellSlots.secondLevel.count - 1
        }
      }
    }

    "spend the lowest available spell slot if using a higher slot has no benefit" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell =
            trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, higherSpellSlot = false)

          val buffingRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellKnown(trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Ranger]

          val rangerCombatant = buffingRanger.withCombatIndex(1)

          val updatedRanger = castSelfBuffSpell(Priority)(rangerCombatant).update
            .asInstanceOf[Ranger]

          updatedRanger.spellSlots.firstLevel.count shouldBe buffingRanger.spellSlots.firstLevel.count - 1
          updatedRanger.spellSlots.secondLevel.count shouldBe buffingRanger.spellSlots.secondLevel.count
        }
      }
    }

    "meet the condition if the Spell Caster has a Self Buff spell to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 2))
        .withAllSpellSlotsAvailableForLevel(LevelFour)
        .withLevel(LevelFour)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe true
    }

    "meet the condition if the Spell Caster has only a Self Buff cantrip to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 0))
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster does not have a Self Buff spell to cast" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(FireBolt, Fireball)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Self Buff spells at its level" in new TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser

      val wizard = random[Wizard]
        .withSpellsKnown(FireBolt, trackedSelfBuffSpell(HuntersMarkBuffCondition, 3))
        .withSpellSlots(SpellSlots(firstLevelSlots = 4, secondLevelSlots = 3, thirdLevelSlots = 0))
        .withLevel(LevelFive)
        .withCombatIndex(1)

      castSelfBuffSpell(Priority)(wizard).conditionMet shouldBe false
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
