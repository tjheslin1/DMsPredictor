package unit

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.BlessCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{ConditionSpell, Spell, SpellSlots}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
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
            .withLevel(LevelFive)
            .withBaseWeapon(trackedSword)
            .withAbilities(List(extraAttack(Priority)))
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
            .withLevel(LevelFive)
            .withAbilities(
              List(extraAttack(Priority),
                   trackedAbility(2, action = WholeAction),
                   otherTrackedAbility(3, action = SingleAttack, updatedTracking = ())))
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
            .withLevel(LevelFive)
            .withAbilities(
              List(extraAttack(Priority),
                   trackedAbility(2, action = WholeAction),
                   otherTrackedAbility(3, action = SingleAttack)))
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

          val trackedSpell = trackedMeleeSpellAttack(1)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          meleeSpellUsedCount shouldBe 1
          meleeSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedSingleTargetSavingThrowSpell(1, Wisdom)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(10)
            .withCombatIndex(1)

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(monster), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          singleTargetSavingThrowSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot which has a damaging spell" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedSingleTargetSavingThrowSpell(2, Strength)
          val trackedMultiSpell = trackedMultiTargetSavingThrowSpell(2, Wisdom, higherSpellSlot = false)
          val trackedHealSpell = trackedHealingSpell(3)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell, trackedMultiSpell, trackedHealSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(20)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withStrength(1).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(trackedCleric)
            .useAbility(List(goblinCombatant), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 1
          singleTargetSavingThrowSpellLevelUsed shouldBe 3

          multiTargetSavingThrowSpellUsedCount shouldBe 0

          trackedHealingSpellUsedCount shouldBe 0
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSingleTargetSpell = trackedMeleeSpellAttack(1)
          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSingleTargetSpell, trackedMultiTargetSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSingleTargetSpell = trackedMeleeSpellAttack(1, higherSpellSlot = false)
          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(3, Dexterity)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSingleTargetSpell, trackedMultiTargetSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe (trackedCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
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

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetOffensiveSpell(Priority)(clericCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "cast cantrip if defined and no spell slots are available" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMeleeSpellAttack(0)
          val trackedSavingThrowSpell = trackedSingleTargetSavingThrowSpell(1, Wisdom)

          val noSpellSlotsCleric = cleric
            .withSpellsKnown(trackedSpell, trackedSavingThrowSpell)
            .withNoSpellSlotsAvailable()
            .withWisdom(24)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(2).withCombatIndex(2)

          castSingleTargetOffensiveSpell(Priority)(noSpellSlotsCleric)
            .useAbility(List(monster), LowestFirst)

          singleTargetSavingThrowSpellUsedCount shouldBe 0
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
            .withSpellsKnown(trackedMultiTargetSavingThrowSpell(1, Strength),
                             trackedSingleTargetSavingThrowSpell(3, Wisdom))
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
            .withSpellsKnown(trackedMultiTargetSavingThrowSpell(1, Strength), trackedMultiTargetSavingThrowSpell(2, Strength))
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .withCombatIndex(1)

          castSingleTargetOffensiveSpell(Priority)(wizardCombatant).conditionMet shouldBe false
        }
      }
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val lichCombatant = lich
            .withSpellsKnown(trackedMeleeSpellAttack(2), // damage dealt is 4
              trackedMultiTargetSavingThrowSpell(3, Strength, higherSpellSlot = false)
            )
            .withCombatIndex(1)

          val easyToHitFighter = fighter
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withNoArmour()
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            castSingleTargetOffensiveSpell(Priority)(lichCombatant)
              .useAbility(List(easyToHitFighter), LowestFirst)

          meleeSpellUsedCount shouldBe 1
          meleeSpellLevelUsed shouldBe 9

          multiTargetSavingThrowSpellUsedCount shouldBe 0

          updatedFighter.health shouldBe 100 - 4
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

          val trackedSpell = trackedHealingSpell(1, higherSpellSlot = true)
          val trackedAttackSpell = trackedMeleeSpellAttack(1)

          val healingCleric = cleric
            .withSpellsKnown(trackedSpell, trackedAttackSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .withCombatIndex(1)

          val damagedFighter = fighter.withHealth(10).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) =
            castSingleTargetHealingSpell(Priority)(healingCleric)
              .useAbility(List(damagedFighter), LowestFirst)

          trackedHealingSpellUsedCount shouldBe 1
          trackedHealingSpellLevelUsed shouldBe 3

          meleeSpellUsedCount shouldBe 0

          healedFighter.creature.health shouldBe 11
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedHealSpell = trackedHealingSpell(1)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(2, Dexterity, higherSpellSlot = false)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1))
              .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe healingCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe (healingCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(1, higherSpellSlot = false))
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1))
              .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe (healingCleric.spellSlots.firstLevel.count - 1)
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (cleric: Cleric, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric
            .withSpellsKnown(trackedHealingSpell(0), trackedMeleeSpellAttack(1))
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withWisdom(12)
            .asInstanceOf[Cleric]

          val woundedHunter = hunter
            .withHealth(1)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1))
              .useAbility(List(woundedHunter), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe healingCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe healingCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe healingCleric.spellSlots.thirdLevel.count
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

    "target another Monster if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell: Spell = trackedHealingSpell(2)

          val lichCombatant = lich
            .withSpellKnown(trackedSpell)
            .withIntelligence(10)
            .withCombatIndex(1)

          val fighterCombatant = fighter.withCombatIndex(2)

          val damagedGoblin = goblin
            .withHealth(50)
            .withMaxHealth(100)
            .withCombatIndex(3)

          val (_, List(_, Combatant(_, updatedGoblin: Goblin))) =
            castSingleTargetHealingSpell(Priority)(lichCombatant)
              .useAbility(List(fighterCombatant, damagedGoblin), LowestFirst)

          trackedHealingSpellUsedCount shouldBe 1
          trackedHealingSpellLevelUsed shouldBe 9

          updatedGoblin.creature.health shouldBe 50 + 1
        }
      }
    }
  }

  "castConditionSpell" should {
    "cast a spell (condition)" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSpell = trackedConditionSpell(2)

          val trackedCleric = cleric
            .withSpellKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          castConditionSpell(Priority)(trackedCleric)
            .useAbility(List(goblinCombatant), LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 2
        }
      }
    }

    "set the spellCasters concentration to the cast spell for a concentration spell" in {
      forAll { (cleric: Cleric, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(1)

          val trackedSpell = trackedConditionSpell(2, savingThrowAttribute = Wisdom, concentration = true)

          val trackedCleric = cleric
            .withSpellKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withWisdom(20)
            .withCombatIndex(1)

          val monster = goblin.withWisdom(2).withCombatIndex(2)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConditionSpell(Priority)(trackedCleric).useAbility(List(monster), LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 3

          updatedCleric.concentratingSpell shouldBe trackedSpell.some
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiConditionSpell = trackedConditionSpell(1, concentration = false, higherSpellSlot = true)

          val trackedCleric = cleric
            .withSpellsKnown(trackedMultiConditionSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConditionSpell(Priority)(clericCombatant)
              .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the highest available spell slot for a concentration spell" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedConcentrationSpell = trackedConditionSpell(1, concentration = true, higherSpellSlot = true)

          val trackedCleric = cleric
            .withSpellsKnown(trackedConcentrationSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConditionSpell(Priority)(clericCombatant)
              .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe trackedCleric.spellSlots.secondLevel.count
          updatedCleric.spellSlots.thirdLevel.count shouldBe (trackedCleric.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedSpell = trackedConditionSpell(2, concentration = false, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConditionSpell(Priority)(clericCombatant)
              .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe (trackedCleric.spellSlots.secondLevel.count - 1)
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot necessary for a concentration spell which does not benefit from a higher slot" in {
      forAll { (cleric: Cleric, goblinOne: Goblin, goblinTwo: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedConcentrationSpell = trackedConditionSpell(2, concentration = true, higherSpellSlot = false)

          val trackedCleric = cleric
            .withSpellsKnown(trackedConcentrationSpell)
            .withChannelDivinityUsed()
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Cleric]

          val clericCombatant = trackedCleric.withCombatIndex(1)

          val goblinCombatantOne = goblinOne.withCombatIndex(2)
          val goblinCombatantTwo = goblinTwo.withCombatIndex(3)

          val (Combatant(_, updatedCleric: Cleric), _) =
            castConditionSpell(Priority)(clericCombatant)
              .useAbility(List(goblinCombatantOne, goblinCombatantTwo), LowestFirst)

          updatedCleric.spellSlots.firstLevel.count shouldBe trackedCleric.spellSlots.firstLevel.count
          updatedCleric.spellSlots.secondLevel.count shouldBe (trackedCleric.spellSlots.secondLevel.count - 1)
          updatedCleric.spellSlots.thirdLevel.count shouldBe trackedCleric.spellSlots.thirdLevel.count
        }
      }
    }

    "not meet the condition if the Spell Caster has no condition spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withSpellKnown(MagicMissile).withCombatIndex(1)

      castConditionSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoSpellSlotsAvailable()
        .withCombatIndex(1)

      castConditionSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster is concentrating and has no non-concentration condition spells to cast" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val conditionSpell = trackedConditionSpell(spellLvl = 2)

          val concentratingCleric = cleric
            .withSpellsKnown(conditionSpell,
              trackedSingleTargetSavingThrowSpell(1, Wisdom))
            .withConcentratingOn(conditionSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withLevel(LevelThree)
            .withCombatIndex(1)

          castConditionSpell(Priority)(concentratingCleric).conditionMet shouldBe false
        }
      }
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val lichCombatant = lich
            .withSpellKnown(trackedConditionSpell(spellLvl = 2, savingThrowAttribute = Dexterity))
            .withCombatIndex(1)

          val easyToHitFighter = fighter.withDexterity(2).withCombatIndex(2)

          castConditionSpell(Priority)(lichCombatant).useAbility(List(easyToHitFighter),
                                                                     LowestFirst)

          conditionSpellUsedCount shouldBe 1
          conditionSpellLevelUsed shouldBe 9
        }
      }
    }
  }

  "castMultiTargetOffensiveSpell" should {

    "cast a spell using the highest available spell slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(1, Wisdom)
          val trackedSingleSpell = trackedSingleTargetSavingThrowSpell(2, Strength, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedSpell, trackedSingleSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withProficiencyBonus(6)
            .withLevel(LevelFive)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster))) =
            castMultiTargetOffensiveSpell(Priority)(trackedWizard)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedMonsterOne.health < monsterOne.creature.health shouldBe true
          updatedMonsterTwo.health < monsterTwo.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiSavingThrowSpellDamageRollCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 3

          singleTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "cast a spell using the lowest spell slot for a spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(2, Wisdom, higherSpellSlot = false)
          val trackedSingleSpell = trackedSingleTargetSavingThrowSpell(3, Strength, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedSpell, trackedSingleSpell)
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withProficiencyBonus(6)
            .withLevel(LevelThree)
            .withIntelligence(20)
            .withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedMonsterOne: TestMonster), Combatant(_, updatedMonsterTwo: TestMonster))) =
            castMultiTargetOffensiveSpell(Priority)(trackedWizard)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedMonsterOne.health < monsterOne.creature.health shouldBe true
          updatedMonsterTwo.health < monsterTwo.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 2
          multiSavingThrowSpellDamageRollCount shouldBe 1

          singleTargetSavingThrowSpellUsedCount shouldBe 0
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(1, Strength, higherSpellSlot = true)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(2, Dexterity, higherSpellSlot = false)

          val trackedWizard = wizard
            .withSpellsKnown(trackedMultiTargetSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe trackedWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe trackedWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (trackedWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, testMonsterOne: TestMonster, testMonsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedMultiTargetSpell = trackedMultiTargetSavingThrowSpell(1, Strength, higherSpellSlot = false)
          val trackedSingleTargetSpell = trackedMeleeSpellAttack(3)

          val trackedWizard = wizard
            .withSpellsKnown(trackedMultiTargetSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = trackedWizard.withCombatIndex(1)

          val monsterOne = testMonsterOne
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(2)

          val monsterTwo = testMonsterTwo
            .withHealth(100)
            .withMaxHealth(100)
            .withDexteritySavingThrowScore(-10)
            .withCombatIndex(3)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetOffensiveSpell(Priority)(wizardCombatant)
              .useAbility(List(monsterOne, monsterTwo), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (trackedWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe trackedWizard.spellSlots.secondLevel.count
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

    "target Players if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, wizard: Wizard) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val trackedSpell = trackedMultiTargetSavingThrowSpell(2, Dexterity)
          val trackedSingleSpell = trackedMeleeSpellAttack(3, higherSpellSlot = false)

          val lichCombatant = lich
            .withSpellKnown(trackedSpell)
            .withCombatIndex(1)

          val easyToHitFighter = fighter
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withCombatIndex(2)

          val easyToHitWizard = wizard
            .withHealth(100)
            .withMaxHealth(100)
            .withDexterity(2)
            .withCombatIndex(3)

          val (_, List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedWizard: Wizard))) =
            castMultiTargetOffensiveSpell(Priority)(lichCombatant)
              .useAbility(List(easyToHitFighter, easyToHitWizard), LowestFirst)

          updatedFighter.health < easyToHitFighter.creature.health shouldBe true
          updatedWizard.health < easyToHitWizard.creature.health shouldBe true

          multiTargetSavingThrowSpellUsedCount shouldBe 1
          multiTargetSavingThrowSpellLevelUsed shouldBe 9

          meleeSpellUsedCount shouldBe 0
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
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) =
            castSelfBuffSpell(Priority)(rangerCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0
        }
      }
    }

    "set the spellCasters concentration to the cast spell if a concentration spell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, concentration = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) =
            castSelfBuffSpell(Priority)(rangerCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0

          updatedRanger.concentratingSpell shouldBe trackedBuffSpell.some
        }
      }
    }

    "not set the spellCasters concentration to the cast spell if not a concentration spell" in {
      forAll { ranger: Ranger =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, concentration = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val rangerCombatant = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val (Combatant(_, updatedRanger: Ranger), _) =
            castSelfBuffSpell(Priority)(rangerCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 1

          trackedMultiTargetBuffSpellUsedCount shouldBe 0

          updatedRanger.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, higherSpellSlot = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val buffingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSelfBuffSpell(Priority)(wizardCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (buffingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the highest available spell slot for a concentration spell" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, concentration = true, higherSpellSlot = true)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSelfBuffSpell(Priority)(wizardCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe buffingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (buffingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot when using a higher slot has no benefit" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell =
            trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, higherSpellSlot = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSelfBuffSpell(Priority)(wizardCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (buffingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot when using a higher slot has no benefit for a concentration spell" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell =
            trackedSelfBuffSpell(HuntersMarkBuffCondition, 1, concentration = true, higherSpellSlot = false)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val buffingWizard = wizard
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val wizardCombatant = buffingWizard.withCombatIndex(1)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSelfBuffSpell(Priority)(wizardCombatant)
              .useAbility(List.empty[Combatant], LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (buffingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe buffingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe buffingWizard.spellSlots.thirdLevel.count
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

    "cast a spell (Self Buff) for a Monster updating the casters conditions" in {
      forAll { lich: Lich =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 1)
          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(2, BlessCondition(), higherSpellSlot = false)

          val buffingLich = lich
            .withSpellsKnown(trackedBuffSpell, trackedMultiBuffSpell)
            .withCombatIndex(1)

          val (Combatant(_, updatedLich: Lich), _) =
            castSelfBuffSpell(Priority)(buffingLich).useAbility(List.empty[Combatant], LowestFirst)

          selfBuffSpellUsedCount shouldBe 1
          selfBuffSpellLevelUsed shouldBe 9

          trackedMultiTargetBuffSpellUsedCount shouldBe 0
        }
      }
    }
  }

  "castMultiTargetBuffSpell" should {
    "cast a spell (Multi Target Buff) updating the targets conditions" in {
      forAll { (paladin: Paladin, hunter: Hunter, berserker: Berserker) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffTwoTargetsSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), numTargets = 2)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingPaladin = paladin
            .withSpellsKnown(trackedBuffTwoTargetsSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val hunterCombatant = hunter.withCombatIndex(2)

          val berserkerCombatant = berserker.withCombatIndex(3)

          val (Combatant(_, updatedPaladin: Paladin),
            List(Combatant(_, updatedHunter: Hunter), Combatant(_, updatedBerserker: Berserker))) =
              castMultiTargetBuffSpell(Priority)(castingPaladin)
                .useAbility(List(hunterCombatant, berserkerCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.conditions shouldBe List.empty[Condition]

          updatedHunter.conditions shouldBe List(BlessCondition())
          updatedBerserker.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "cast a spell (Multi Target Buff) updating the SpellCasters own conditions" in {
      forAll { (paladin: Paladin, hunter: Hunter, berserker: Berserker) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedBuffThreeTargetsSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), numTargets = 3)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingPaladin = paladin
            .withSpellsKnown(trackedBuffThreeTargetsSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val hunterCombatant = hunter.withCombatIndex(2)

          val berserkerCombatant = berserker.withCombatIndex(3)

          val (Combatant(_, updatedPaladin: Paladin),
            List(Combatant(_, updatedHunter: Hunter), Combatant(_, updatedBerserker: Berserker))) =
              castMultiTargetBuffSpell(Priority)(castingPaladin)
                .useAbility(List(hunterCombatant, berserkerCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.conditions shouldBe List(BlessCondition())

          updatedHunter.conditions shouldBe List(BlessCondition())
          updatedBerserker.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "target Monsters if caster is a Monster" in {
      forAll { (lich: Lich, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition())
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingLich = lich
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          val (Combatant(_, updatedLich: Lich), List(Combatant(_, updatedGoblin: Goblin))) =
            castMultiTargetBuffSpell(Priority)(castingLich)
              .useAbility(List(goblinCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 9

          selfBuffSpellUsedCount shouldBe 0

          updatedLich.conditions shouldBe List(BlessCondition())

          updatedGoblin.conditions shouldBe List(BlessCondition())
        }
      }
    }

    "set the spellCasters concentration to the cast spell if a concentration spell" in {
      forAll { (paladin: Paladin, champion: Champion) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = true)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val paladinCombatant = paladin
            .withSpellKnown(trackedMultiBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val championCombatant = champion.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) =
            castMultiTargetBuffSpell(Priority)(paladinCombatant)
              .useAbility(List(championCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.concentratingSpell shouldBe trackedMultiBuffSpell.some
        }
      }
    }

    "not set the spellCasters concentration to the cast spell if not a concentration spell" in {
      forAll { (paladin: Paladin, champion: Champion) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = false)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val paladinCombatant = paladin
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withCombatIndex(1)

          val championCombatant = champion.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) =
            castMultiTargetBuffSpell(Priority)(paladinCombatant)
              .useAbility(List(championCombatant), LowestFirst)

          trackedMultiTargetBuffSpellUsedCount shouldBe 1
          trackedMultiTargetBuffSpellLevelUsed shouldBe 1

          selfBuffSpellUsedCount shouldBe 0

          updatedPaladin.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), higherSpellSlot = true)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingWizard = wizard
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetBuffSpell(Priority)(castingWizard.withCombatIndex(1))
              .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe castingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (castingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the highest available spell slot for concentration spell" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = true, higherSpellSlot = true)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingWizard = wizard
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetBuffSpell(Priority)(castingWizard.withCombatIndex(1))
              .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe castingWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (castingWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot if using a higher slot has no benefit" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), higherSpellSlot = false)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetBuffSpell(Priority)(castingWizard.withCombatIndex(1))
              .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (castingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe castingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "spend the lowest available spell slot if using a higher slot has no benefit for a concentration spell" in {
      forAll { (wizard: Wizard, paladin: Paladin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedMultiBuffSpell = trackedMultiTargetBuffSpell(1, BlessCondition(), concentration = true, higherSpellSlot = false)
          val trackedBuffSpell = trackedSelfBuffSpell(HuntersMarkBuffCondition, 2, higherSpellSlot = false)

          val castingWizard = wizard
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withSpellsKnown(trackedMultiBuffSpell, trackedBuffSpell)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val paladinCombatant = paladin.withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castMultiTargetBuffSpell(Priority)(castingWizard.withCombatIndex(1))
              .useAbility(List(paladinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (castingWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe castingWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe castingWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "be triggered if the caster has at least one ally to buff" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withSpellKnown(trackedSpell)
        .withLevel(LevelTwo)
        .withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(3)

      castMultiTargetBuffSpell(Priority)(paladin).triggerMet(List(rogue, goblin)) shouldBe true
    }

    "be triggered for a Monster if the caster has at least one ally to buff" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val lich = random[Lich]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(3)

      castMultiTargetBuffSpell(Priority)(lich).triggerMet(List(goblin, rogue)) shouldBe true
    }

    "not be triggered if the caster does not have any allies to buff" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withSpellKnown(trackedSpell)
        .withLevel(LevelTwo)
        .withCombatIndex(2)

      val goblin = random[Goblin].withCombatIndex(2)

      castMultiTargetBuffSpell(Priority)(paladin).triggerMet(List(goblin)) shouldBe false
    }

    "not be triggered for a Monster if the caster does not have any allies to buff" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val lich = random[Lich]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(2)

      val rogue = random[Rogue].withCombatIndex(2)

      castMultiTargetBuffSpell(Priority)(lich).triggerMet(List(rogue)) shouldBe false
    }

    "meet the condition if the Spell Caster has a Multi Target Buff cantrip to cast" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(0, BlessCondition())

      val cleric = random[Cleric]
        .withSpellKnown(trackedSpell)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(cleric).conditionMet shouldBe true
    }

    "meet the condition if the Spell Caster has a Multi Target Buff spell to cast" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedSpell = trackedMultiTargetBuffSpell(1, BlessCondition())

      val paladin = random[Paladin]
        .withSpellKnown(trackedSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(paladin).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster cannot cast any Multi Target Buff spells at its level" in new TestContext {
      override implicit val roll: RollStrategy = Dice.defaultRandomiser

      val trackedMeleeAttackSpell = trackedMultiTargetSavingThrowSpell(1, Strength)

      val paladin = random[Paladin]
        .withSpellKnown(trackedMeleeAttackSpell)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      castMultiTargetBuffSpell(Priority)(paladin).conditionMet shouldBe false
    }
  }

  "castSingleTargetInstantEffectSpell" should {

    "cast a spell (Instant Effect) using the highest available spell slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, higherSpellSlot = true)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          castSingleTargetInstantEffectSpell(1)(instantEffectWizard).useAbility(List(goblinCombatant), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 3
        }
      }
    }

    "cast a spell (Instant Effect) using the lowest available spell slot necessary" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, setHealthToOneEffect, higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard).useAbility(List(goblinCombatant), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, setHealthToOneEffect, higherSpellSlot = true)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(2, Dexterity, higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellsKnown(trackedInstantSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard.withCombatIndex(1))
                .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe instantEffectWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe (instantEffectWizard.spellSlots.thirdLevel.count - 1)
        }
      }
    }

    "spend the lowest available spell slot necessary for spell which does not benefit from a higher slot" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, setHealthToOneEffect, higherSpellSlot = false)
          val trackedSingleTargetSpell = trackedSingleTargetSavingThrowSpell(2, Dexterity, higherSpellSlot = false)

          val instantEffectWizard = wizard
            .withSpellsKnown(trackedInstantSpell, trackedSingleTargetSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard.withCombatIndex(1))
              .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe (instantEffectWizard.spellSlots.firstLevel.count - 1)
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe instantEffectWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "not spend a spell slot if cantrip was found and used" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(0, setHealthToOneEffect, higherSpellSlot = true)

          val instantEffectWizard = wizard
            .withSpellKnown(trackedInstantSpell)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (Combatant(_, updatedWizard: Wizard), _) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard.withCombatIndex(1))
              .useAbility(List(goblinCombatant), LowestFirst)

          updatedWizard.spellSlots.firstLevel.count shouldBe instantEffectWizard.spellSlots.firstLevel.count
          updatedWizard.spellSlots.secondLevel.count shouldBe instantEffectWizard.spellSlots.secondLevel.count
          updatedWizard.spellSlots.thirdLevel.count shouldBe instantEffectWizard.spellSlots.thirdLevel.count
        }
      }
    }

    "meet the condition if the Spell Caster has an Instant Effect spell to cast at a level it can cast" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val buffSpellWizard = random[Wizard]
        .withSpellKnown(trackedInstantEffectSpell(1))
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(buffSpellWizard).conditionMet shouldBe true
    }

    "not meet the condition if the Spell Caster has no Instant Effect spells to cast" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val buffSpellWizard = random[Wizard]
        .withSpellKnown(trackedSelfBuffSpell(HuntersMarkBuffCondition, 1))
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(buffSpellWizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val noSpellsWizard = random[Wizard]
        .withSpellsKnown(List.empty[Spell]: _*)
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(noSpellsWizard).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster cannot cast any Instant Effect spells at its level" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val noSpellsWizard = random[Wizard]
        .withSpellsKnown(trackedMeleeSpellAttack(1), trackedInstantEffectSpell(2, setHealthToOneEffect))
        .withAllSpellSlotsAvailableForLevel(LevelOne)
        .withLevel(LevelOne)
        .withCombatIndex(1)

      castSingleTargetInstantEffectSpell(1)(noSpellsWizard).conditionMet shouldBe false
    }

    "target Player if caster is a Monster" in {
      forAll { (lich: Lich, fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val trackedInstantSpell = trackedInstantEffectSpell(1, setHealthToOneEffect, higherSpellSlot = true)

          val instantEffectWizard = lich
            .withSpellKnown(trackedInstantSpell)
            .withCombatIndex(1)

          val (_, List(Combatant(_, updatedFighter: Fighter), Combatant(_, updatedGoblin: Goblin))) =
            castSingleTargetInstantEffectSpell(1)(instantEffectWizard)
              .useAbility(List(fighter.withCombatIndex(2), goblin.withCombatIndex(3)), LowestFirst)

          trackedInstantEffectUsedCount shouldBe 1
          trackedInstantEffectSpellLevelUsed shouldBe 9

          updatedFighter.health shouldBe 1
          updatedGoblin.health shouldBe goblin.health
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
