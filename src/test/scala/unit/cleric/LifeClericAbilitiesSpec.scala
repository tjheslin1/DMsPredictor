package unit.cleric

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.castSingleTargetHealingSpell
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.LifeClericAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
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
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withSpellKnown(trackedHealingSpell(2))
            .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
            .withLevel(LevelThree)
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

          val trackedLevelTwoHealingSpell = trackedHealingSpell(2)

          val lifeCleric = cleric
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withSpellKnown(trackedLevelTwoHealingSpell)
            .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val weakFighter = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            discipleOfLife(1)(lifeCleric).useAbility(List(weakFighter), LowestFirst)

          val trackedSpellHealing = 1
          val expectedHealth = weakFighter.creature.health + trackedSpellHealing +
            discipleOfLifeBonusHealing(trackedLevelTwoHealingSpell.spellLevel)

          updatedFighter.health shouldBe expectedHealth
        }
      }
    }

    "spend the spend slot used by healing spell delegated to" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val lifeCleric = cleric
            .withAllSpellSlotsAvailableForLevel(LevelThree)
            .withSpellKnown(trackedHealingSpell(2))
            .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
            .withLevel(LevelThree)
            .asInstanceOf[Cleric]

          val updatedCleric =
            discipleOfLife(Priority)(lifeCleric.withCombatIndex(1)).update.asInstanceOf[Cleric]

          updatedCleric.spellSlots.secondLevel.count shouldBe (lifeCleric.spellSlots.secondLevel.count - 1)
        }
      }
    }

    "be triggered when a healing spell is triggered (players health is below 50%)" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val lifeCleric = random[Cleric]
        .withSpellKnown(trackedHealingSpell(2))
        .withAbilities(List(discipleOfLife(1), castSingleTargetHealingSpell(2)))
        .withCombatIndex(1)

      val weakFighter = random[Fighter].withHealth(10).withMaxHealth(100).withCombatIndex(2)

      discipleOfLife(1)(lifeCleric).triggerMet(List(weakFighter)) shouldBe true
    }

    "not be triggered when a healing spell is not triggered (players health is below 50%)" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(10)

      val lifeCleric = random[Cleric]
        .withSpellKnown(trackedHealingSpell(2))
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

  "preserveLife" should {
    "restore a number of hit points equal to five times your cleric level" in {
      preserveLifeHealing(LevelTwo) shouldBe 10
      preserveLifeHealing(LevelFive) shouldBe 25
    }

    "restore up to 50% of an allies health using all points" in {
      forAll { (cleric: Cleric, fighter: Fighter, barbarian: Barbarian) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val weakFighter   = fighter.withHealth(10).withMaxHealth(100).withCombatIndex(2)
          val weakBarbarian = barbarian.withHealth(10).withMaxHealth(100).withCombatIndex(3)

          val (_,
               List(Combatant(_, updatedFighter: Fighter),
                    Combatant(_, updatedBarbarian: Barbarian))) =
            preserveLife(Priority)(cleric.withLevel(LevelTwo).withCombatIndex(1))
              .useAbility(List(weakFighter, weakBarbarian), LowestFirst)

          updatedFighter.health shouldBe 20
          updatedBarbarian.health shouldBe 10
        }
      }
    }

    "restore up to 50% of an allies health using as many points as allowed" in {
      forAll { (cleric: Cleric, fighter: Fighter, barbarian: Barbarian, champion: Champion) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val healthyFighter = fighter.withHealth(100).withMaxHealth(100).withCombatIndex(2)
          val weakBarbarian  = barbarian.withHealth(10).withMaxHealth(25).withCombatIndex(3)
          val weakChampion   = champion.withHealth(10).withMaxHealth(25).withCombatIndex(4)

          val (_,
               List(_,
                    Combatant(_, updatedBarbarian: Barbarian),
                    Combatant(_, updatedChampion: Champion))) =
            preserveLife(Priority)(cleric.withLevel(LevelFive).withCombatIndex(1))
              .useAbility(List(healthyFighter, weakBarbarian, weakChampion), LowestFirst)

          updatedBarbarian.health shouldBe 12
          updatedChampion.health shouldBe 12
        }
      }
    }

    "trigger when when there is one ally" in {
      val cleric = random[Cleric].withLevel(LevelTwo).withCombatIndex(1)

      val healthyFighter = random[Fighter].withHealth(20).withMaxHealth(100).withCombatIndex(2)

      preserveLife(Priority)(cleric)
        .triggerMet(List(healthyFighter)) shouldBe true
    }

    "trigger when more than half of allies are below half health" in {
      val cleric = random[Cleric].withLevel(LevelTwo).withCombatIndex(1)

      val healthyFighter = random[Fighter].withHealth(100).withMaxHealth(100).withCombatIndex(2)
      val weakBarbarian  = random[Barbarian].withHealth(10).withMaxHealth(25).withCombatIndex(3)
      val weakChampion   = random[Champion].withHealth(10).withMaxHealth(25).withCombatIndex(4)

      preserveLife(Priority)(cleric)
        .triggerMet(List(healthyFighter, weakBarbarian, weakChampion)) shouldBe true
    }

    "not trigger when more than half of allies are above half health" in {
      val cleric = random[Cleric].withLevel(LevelTwo).withCombatIndex(1)

      val healthyFighter = random[Fighter].withHealth(100).withMaxHealth(100).withCombatIndex(2)
      val weakBarbarian  = random[Barbarian].withHealth(25).withMaxHealth(25).withCombatIndex(3)
      val weakChampion   = random[Champion].withHealth(10).withMaxHealth(25).withCombatIndex(4)

      preserveLife(Priority)(cleric)
        .triggerMet(List(healthyFighter, weakBarbarian, weakChampion)) shouldBe false
    }

    "not be used if Channel Divinity is already used" in {
      val cleric = random[Cleric].withChannelDivinityUsed().withCombatIndex(1)

      preserveLife(Priority)(cleric).conditionMet shouldBe false
    }

    "update the BaseCleric's channelDivinityUsed to true" in {
      val cleric = random[Cleric].withCombatIndex(1)

      val updatedCleric = preserveLife(Priority)(cleric).update.asInstanceOf[BaseCleric]

      updatedCleric.channelDivinityUsed shouldBe true
    }
  }

  "restoreHealthUsingPool" should {
    "use as many points as possible in the pool to heal allies to 50% of their max hp" in {
      val healthyFighter = random[Fighter].withHealth(100).withMaxHealth(100).withCombatIndex(1)
      val weakBarbarian  = random[Barbarian].withHealth(25).withMaxHealth(80).withCombatIndex(2)
      val weakChampion   = random[Champion].withHealth(10).withMaxHealth(30).withCombatIndex(3)
      val weakCleric     = random[Cleric].withHealth(80).withMaxHealth(200).withCombatIndex(4)

      val List(Combatant(_, updatedFighter: Fighter),
               Combatant(_, updatedBarbarian: Barbarian),
               Combatant(_, updatedChampion: Champion),
               Combatant(_, updatedCleric: Cleric)) =
        restoreHealthUsingPool(20, List(healthyFighter, weakBarbarian, weakChampion, weakCleric))

      updatedFighter.health shouldBe 100
      updatedBarbarian.health shouldBe 40
      updatedChampion.health shouldBe 15
      updatedCleric.health shouldBe 80
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var trackedHealingSpellUsed = false
    def trackedHealingSpell(spellLvl: SpellLevel): Spell = new SingleTargetHealingSpell {
      val name: String                   = "tracked-healing-spell"
      val school: SchoolOfMagic          = Evocation
      val castingTime: CastingTime       = OneAction
      val spellLevel: SpellLevel         = spellLvl
      val requiresConcentration: Boolean = false

      def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
        trackedHealingSpellUsed = true
        1
      }
    }
  }
}
