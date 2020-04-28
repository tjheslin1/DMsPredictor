package unit.wizard

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{AcidArrowCondition, Condition, Stunned}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSaveDc
import io.github.tjheslin1.dmspredictor.model.spellcasting.{FirstLevelSpellSlots, Spell}
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class WizardSpellsSpec extends UnitSpecBase {

  "Fire Bolt" should {
    "deal 1d10 damage for a first level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelOne)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 10
    }

    "deal 1d10 damage for a second level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelTwo)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 10
    }

    "deal 2d10 damage for a fifth level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelFive)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 20
    }

    "deal 3d10 damage for a eleventh level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelEleven)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 30
    }

    "deal 4d10 damage for a seventeenth level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelSeventeen)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 40
    }

    "deal 4d10 damage for a twenty level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val wizard = random[Wizard]
        .withSpellKnown(FireBolt)
        .withLevel(LevelTwenty)
        .asInstanceOf[Wizard]

      FireBolt.damage(wizard, FireBolt.spellLevel) shouldBe 40
    }
  }

  "acid arrow" should {

    "apply acid arrow condition on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(18, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.conditions shouldBe List(AcidArrowCondition(2))
        }
      }
    }

    "deal damage twice on critical hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(20, // attack roll
            2, 2, 2, 2, 2, 2, 2, 2) // damage rolls (4d4 twice)
          // format: n
          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 34
        }
      }
    }

    "deal damage on hit" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(18, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(10).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 42
        }
      }
    }

    "deal half damage on miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(2, // attack roll
                                   2, 2, 2, 2) // damage rolls (4d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 46
        }
      }
    }

    "deal no damage on critical miss" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(0)

          val levelFourWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFour)
            .withLevel(LevelFour)
            .asInstanceOf[Wizard]

          val goblinCombatant = goblin.withArmourClass(20).withHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFourWizard, 2, List(goblinCombatant))

          updatedGoblin.health shouldBe 50
        }
      }
    }

    "be cast using the highest spell slot available" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          // format: off
          val diceRolls = Iterator(19, // attack roll
            2, 2, 2, 2, 2) // damage rolls (5d4)
          // format: on

          implicit val rollStrategy: RollStrategy = _ => RollResult(diceRolls.next())

          val levelFiveWizard = wizard
            .withSpellKnown(AcidArrow)
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withLevel(LevelFive)
            .asInstanceOf[Wizard]

          val fiftyHpGoblin = goblin
            .withArmourClass(1)
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            AcidArrow.effect(levelFiveWizard, 3, List(fiftyHpGoblin))

          updatedGoblin.health shouldBe 40
        }
      }
    }
  }

  "shield spell" should {
    "use a first level spell slot if available" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val ac13Wizard =
            wizard.withMageArmourPrepared(true).withDexterity(10).asInstanceOf[Wizard]

          val (_, updatedSpellCaster: SpellCaster) =
            ShieldSpell.updateAttackOnReaction(ac13Wizard, 15)

          val expectedSpellSlots = ac13Wizard.spellSlots.copy(
            firstLevel = FirstLevelSpellSlots(ac13Wizard.spellSlots.firstLevel.count - 1))

          updatedSpellCaster.spellSlots shouldBe expectedSpellSlots
        }
      }
    }

    "update the casters reaction to be used" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val reactionNotUsedWizard = wizard
            .withDexterity(10)
            .withReactionUsed(false)

          val (_, updatedSpellCaster: SpellCaster) =
            ShieldSpell.updateAttackOnReaction(reactionNotUsedWizard, 15)

          updatedSpellCaster.reactionUsed shouldBe true
        }
      }
    }

    "return the original attack result if no first level spell slots are available" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val noSpellSlotsWizard =
            wizard.withMageArmourPrepared(false).withNoSpellSlotsAvailable().withDexterity(10)

          ShieldSpell.updateAttackOnReaction(noSpellSlotsWizard, 15) shouldBe (Hit, noSpellSlotsWizard)
        }
      }
    }

    "return the original attack result if the attack missed" in {
      forAll { wizard: Wizard =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          ShieldSpell.updateAttackOnReaction(wizard, 0) shouldBe (Miss, wizard)
        }
      }
    }

    "no longer be in effect at the start of the casters next turn" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val ac13Wizard = wizard.withMageArmourPrepared(true).withDexterity(10)
          val (attackResult, shieldSpellActiveWizard: Wizard) =
            ShieldSpell.updateAttackOnReaction(ac13Wizard, 15)

          attackResult shouldBe Miss // checking Shield was activated

          val goblinCombatant = goblin.withCombatIndex(2)

          val Queue(_, Combatant(_, updatedWizard: Wizard)) =
            Move.takeMove(Queue(shieldSpellActiveWizard.withCombatIndex(1), goblinCombatant),
                          LowestFirst)

          updatedWizard.conditions shouldBe List.empty[Condition]
        }
      }
    }
  }

  "blight spell" should {
    "deal 8d8 damage at 4th level" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val lich = random[Lich]

      Blight.damage(lich, 4) shouldBe 64 // 8 * 8
    }

    "deal 10d8 damage at 6th level" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val lich = random[Lich]

      Blight.damage(lich, 6) shouldBe 80 // 10 * 8
    }
  }

  "Disintegrate spell" should {
    "deal 10d6 + 40 damage at 6th level on a hit" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(6)

      val lich = random[Lich]

      Disintegrate.damage(lich, 6) shouldBe 60 + 40 // (10 * 6) + 40
    }

    "deal 16d6 + 40 damage at 8th level on a hit" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(6)

      val lich = random[Lich]

      Disintegrate.damage(lich, 8) shouldBe 96 + 40 // (16 * 6) + 40
    }

    "kill the creature outright if it drops their health to 0" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(10)

      val lich = random[Lich]

      val lowDexFighter = random[Fighter]
        .withHealth(10)
        .withMaxHealth(10)
        .withDexterity(2)
        .withCombatIndex(1)

      val (_, List(Combatant(_, updatedFighter: Fighter))) =
        Disintegrate.effect(lich, Disintegrate.spellLevel, List(lowDexFighter))

      updatedFighter.health shouldBe 0
      updatedFighter.isAlive shouldBe false
    }

    // copy of SingleTargetSavingThrowSpellSpec
    "deal full damage if saving throw failed" in {
      forAll { (lich: Lich, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val trackedLich = lich
            .withSpellKnown(Disintegrate)
            .asInstanceOf[Lich]

          val monster = testMonster
            .withHealth(500)
            .withMaxHealth(500)
            .withSavingThrowScores(dexterity = -4)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            Disintegrate.effect(trackedLich, Disintegrate.spellLevel, List(monster))

          updatedMonster.health shouldBe monster.creature.health - 140 // 10d10 + 40 - using RollResult(10)
        }
      }
    }

    // copy of SingleTargetSavingThrowSpellSpec
    "deal no damage if saving throw passed and half damage on save is false" in {
      forAll { (lich: Lich, testMonster: TestMonster) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val trackedLich = lich
            .withSpellKnown(Disintegrate)
            .withIntelligence(10)
            .asInstanceOf[Lich]

          val monster = testMonster
            .withDexteritySavingThrowScore(10)
            .withHealth(100)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: TestMonster))) =
            Disintegrate.effect(trackedLich, Disintegrate.spellLevel, List(monster))

          updatedMonster.health shouldBe monster.creature.health
        }
      }
    }
  }

  "Finger of Death spell" should {
    "deal 7d8 + 30 damage" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val lich = random[Lich]

      FingerOfDeath.damage(lich, 7) shouldBe 86
    }

    "spawn a Zombie if target is outright killed" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val fingerOfDeathLich = lich
            .withSpellKnown(FingerOfDeath)

          val lowHealthFighter =
            fighter
              .withConstitution(2)
              .withHealth(2)
              .withMaxHealth(10)
              .withCombatIndex(2)

          val (_, others) =
            FingerOfDeath.effect(fingerOfDeathLich,
                                 FingerOfDeath.spellLevel,
                                 List(lowHealthFighter))

          others.size shouldBe 2

          val List(zombie, Combatant(_, updatedFighter: Fighter)) = others

          updatedFighter.isAlive shouldBe false

          zombie.index shouldBe 3
        }
      }
    }

    "not spawn a Zombie if target is made unconscious but not killed" in {
      forAll { (lich: Lich, fighter: Fighter) =>
        new TestContext {
          implicit val rollStrategy: RollStrategy = _ => RollResult(10)

          val fingerOfDeathLich = lich
            .withSpellKnown(FingerOfDeath)

          val lowConstitutionFighter =
            fighter
              .withConstitution(2)
              .withHealth(20)
              .withMaxHealth(200)
              .withCombatIndex(2)

          val (_, others) =
            FingerOfDeath.effect(fingerOfDeathLich,
                                 FingerOfDeath.spellLevel,
                                 List(lowConstitutionFighter))

          others.size shouldBe 1

          val List(Combatant(_, updatedFighter: Fighter)) = others

          updatedFighter.isAlive shouldBe true
          updatedFighter.health shouldBe 0
        }
      }
    }
  }

  "Power Word Stun spell" should {
    "apply the Stunned condition if the target has 150 hit points or fewer" in {
      new TestContext {
        override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

        val lich = random[Lich].withSpellKnown(PowerWordStun)

        val rogue = random[Rogue].withHealth(90).withMaxHealth(200).withCombatIndex(2)

        val (_, List(Combatant(_, updatedRogue: Rogue))) =
          PowerWordStun.effect(lich, PowerWordStun.spellLevel, List(rogue))

        val saveDC = spellSaveDc(lich)
        updatedRogue.conditions should contain theSameElementsAs List(Stunned(saveDC))
      }
    }

    "not apply the Stunned condition if the target has more than 150 hit points" in {
      new TestContext {
        override implicit val rollStrategy: RollStrategy = _ => RollResult(10)

        val lich = random[Lich].withSpellKnown(PowerWordStun)

        val rogue = random[Rogue].withHealth(170).withMaxHealth(200).withCombatIndex(2)

        val (_, List(Combatant(_, updatedRogue: Rogue))) =
          PowerWordStun.effect(lich, PowerWordStun.spellLevel, List(rogue))

        updatedRogue.conditions should contain theSameElementsAs List.empty[Condition]
      }
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
