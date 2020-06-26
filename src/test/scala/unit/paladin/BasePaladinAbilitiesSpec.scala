package unit.paladin

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladin.paladinSpellSlots
import io.github.tjheslin1.dmspredictor.classes.paladin.BasePaladinAbilities._
import io.github.tjheslin1.dmspredictor.classes.paladin._
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.equipment.weapons.{PlusOneShortsword, Shortsword}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BasePaladinAbilitiesSpec extends UnitSpecBase {

  "Lay on Hands" should {
    "meet the condition if the Paladin has points left in its Lay on Hands pool" in {
      val paladin = random[Paladin].withLayOnHandsPoolOf(5).withCombatIndex(1)

      layOnHands(8)(paladin).conditionMet shouldBe true
    }

    "not meet the condition if the Paladin has no points left in its Lay on Hands pool" in {
      val noLayOnHandsPaladin = random[Paladin].withLayOnHandsPoolOf(0).withCombatIndex(1)

      layOnHands(1)(noLayOnHandsPaladin).conditionMet shouldBe false
    }

    "be triggered if an ally is below half their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(22).withMaxHealth(50).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe true
      }
    }

    "not be triggered if no allies are below half their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(10).withMaxHealth(10).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe false
      }
    }

    "not be triggered if no allies are alive" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>

        val paladinCombatant = paladin.withCombatIndex(1)

        val fullHealthHunter = hunter.withHealth(0).withMaxHealth(10).withIsAlive(false).withCombatIndex(2)

        layOnHands(1)(paladinCombatant).triggerMet(List(fullHealthHunter)) shouldBe false
      }
    }

    "updated the Paladin's Lay on Hands pool" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val paladinCombatant = paladin.withLayOnHandsPoolOf(20).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(4).withMaxHealth(10).withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedPaladin.layOnHandsPool shouldBe 14
        }
      }
    }

    "heal the target for up to their max hit points" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val paladinCombatant = paladin.withLayOnHandsPoolOf(20).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(4).withMaxHealth(10).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 10
        }
      }
    }

    "heal the target using all remaining points in the Lay on Hands pool" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(10).withMaxHealth(30).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 15
        }
      }
    }

    "bring an unconscious ally back to consciousness" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val unconsciousHunter = hunter.withHealth(0).withMaxHealth(30).withIsAlive(true).withCombatIndex(2)

          val (_, List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(unconsciousHunter), LowestFirst)

          updatedHunter.health shouldBe 5
        }
      }
    }

    "not bring a dead ally back to life" in {
      forAll { (paladin: Paladin, hunter: Hunter) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val paladinCombatant = paladin.withLayOnHandsPoolOf(5).withCombatIndex(1)

          val lowHealthHunter = hunter.withHealth(0).withMaxHealth(30).withIsAlive(false).withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), List(Combatant(_, updatedHunter: Hunter))) =
            layOnHands(1)(paladinCombatant).useAbility(List(lowHealthHunter), LowestFirst)

          updatedHunter.health shouldBe 0
          updatedHunter.isAlive shouldBe false

          updatedPaladin.layOnHandsPool shouldBe 5
        }
      }
    }
  }

  "Divine Smite" should {
    "deal 2d8 extra radiant damage on a weapon attack using a first level spell slot" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(goblinCombatant), LowestFirst)

          updatedGoblin.health shouldBe 50 - (10 + 4 + 20) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "deal 4d8 extra radiant damage on a weapon attack using a third level spell slot" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

         val thirdLevelSpellPaladin = paladin
           .withSpellSlots(SpellSlots(0, 0, 1))
           .withNoFightingStyles()
            .withProficiencyBonus(6)
           .withStrength(18)
           .withDexterity(10)
           .withBaseWeapon(Shortsword)
           .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(100).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(thirdLevelSpellPaladin).useAbility(List(goblinCombatant), LowestFirst)

          updatedGoblin.health shouldBe 100 - (10 + 4 + 40) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "deal a maximum of 5d8  extra radiant damage" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val fifthLevelSpellPaladin = paladin
            .withSpellSlots(SpellSlots(0, 0, 0, 0, 1, 0, 0, 0, 0))
            .withNoFightingStyles()
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val werewolfCombatant = goblin
            .withHealth(100).withMaxHealth(100).withCombatIndex(2)

          val (_, List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(fifthLevelSpellPaladin).useAbility(List(werewolfCombatant), LowestFirst)

          updatedGoblin.health shouldBe 100 - (10 + 4 + 50) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "deal an extra 1d8 radiant damage on top of the regular extra damage against an Undead target" in {
      forAll { (paladin: Paladin, zombie: Zombie) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val zombieCombatant = zombie
            .withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, updatedZombie: Zombie))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(zombieCombatant), LowestFirst)

          updatedZombie.health shouldBe 50 - (10 + 4 + 30) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "deal an extra 1d8 radiant damage on top of the regular extra damage against an Fiend target" in {
      forAll { (paladin: Paladin, testMonster: TestMonster) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val fiendCombatant = testMonster
              .withCreatureType(Fiend)
            .withArmourClass(5)
            .withHealth(50)
            .withMaxHealth(50)
            .withCombatIndex(2)

          val (_, List(Combatant(_, updatedTestMonster: TestMonster))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(fiendCombatant), LowestFirst)

          updatedTestMonster.health shouldBe 50 - (10 + 4 + 30) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "be used in conjunction with Extra Attack" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext with Tracking {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelFivePaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withNoFightingStyles()
            .withSpellsKnown() // to prevent casting other spells
            .withLevel(LevelFive)
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(trackedSword)
            .withCombatIndex(1)

          val goblinCombatant = goblin
            .withHealth(100).withMaxHealth(100).withCombatIndex(2)

          val Queue(Combatant(_, updatedGoblin: Goblin), Combatant(_, updatedPaladin: Paladin)) =
            Move.takeMove(Queue(levelFivePaladin, goblinCombatant), LowestFirst)

          swordUsedCount shouldBe 2

          updatedGoblin.health shouldBe 100 - (1 + 4 + 30) - (1 + 4 + 30) // weapon damage + strength mod + radiant damage
        }
      }
    }

    "not be used if the Paladin kills the creature with the regular weapon attack" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext with Tracking {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(6)
            .withStrength(8)
            .withDexterity(8)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withHealth(1).withMaxHealth(1).withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(goblinCombatant), LowestFirst)

          updatedGoblin.health shouldBe 0

          updatedPaladin.spellSlots shouldBe paladinSpellSlots(LevelTwo)
        }
      }
    }

    "not be used if the Paladin misses its attack" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext with Tracking {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(2)
            .withStrength(10)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(goblinCombatant), LowestFirst)

          updatedPaladin.spellSlots shouldBe paladinSpellSlots(LevelTwo)

          updatedGoblin.health shouldBe goblin.health
        }
      }
    }

    "not be used if the Paladin misses its attack with a CriticalMiss" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext with Tracking {
          implicit val roll: RollStrategy = _ => RollResult(1)

          val levelTwoPaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withNoFightingStyles()
            .withLevel(LevelTwo)
            .withProficiencyBonus(2)
            .withStrength(10)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .withCombatIndex(1)

          val goblinCombatant = goblin.withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), List(Combatant(_, updatedGoblin: Goblin))) =
            divineSmite(1)(levelTwoPaladin).useAbility(List(goblinCombatant), LowestFirst)

          updatedPaladin.spellSlots shouldBe paladinSpellSlots(LevelTwo)

          updatedGoblin.health shouldBe goblin.health
        }
      }
    }

    "spend the highest level spell slot when available" in {
      forAll { (paladin: Paladin, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val levelFivePaladin = paladin
            .withAllSpellSlotsAvailableForLevel(LevelFive)
            .withNoFightingStyles()
            .withLevel(LevelFive)
            .withProficiencyBonus(6)
            .withStrength(18)
            .withDexterity(10)
            .withBaseWeapon(Shortsword)
            .asInstanceOf[Paladin]

          val goblinCombatant = goblin
            .withHealth(100)
            .withMaxHealth(100)
            .withCombatIndex(2)

          val (Combatant(_, updatedPaladin: Paladin), _) =
            divineSmite(1)(levelFivePaladin.withCombatIndex(1))
              .useAbility(List(goblinCombatant), LowestFirst)

          updatedPaladin.spellSlots.firstLevel.count shouldBe levelFivePaladin.spellSlots.firstLevel.count
          updatedPaladin.spellSlots.secondLevel.count shouldBe levelFivePaladin.spellSlots.secondLevel.count - 1
        }
      }
    }

    "meet the condition if the Paladin is level two or higher" in {
      val paladin = random[Paladin]
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      divineSmite(1)(paladin).conditionMet shouldBe true
    }

    "not meet the condition if the Paladin is level one" in {
      val paladin = random[Paladin].withLevel(LevelOne).withCombatIndex(1)

      divineSmite(1)(paladin).conditionMet shouldBe false
    }

    "not meet the condition if the Paladin has no spell slots available" in {
      val paladin = random[Paladin]
        .withNoAvailableSpellsSlots()
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      divineSmite(1)(paladin).conditionMet shouldBe false
    }
  }

  "SacredWeaponCondition" should {
    "make the Paladins weapon to have DamageType of Magical" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val paladin = random[Paladin]
          .withNoFightingStyles()
          .withBaseWeapon(Shortsword)
          .withCondition(SacredWeaponCondition())

        paladin.weapon.damageType shouldBe Magical
      }
    }

    "make the Paladins weapon have a hit bonus equal to its Charisma modifier (minimum of 1)" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val paladin = random[Paladin]
          .withNoFightingStyles()
          .withBaseWeapon(Shortsword)
          .withCondition(SacredWeaponCondition())

        val charismaBonus = Math.max(1, Modifier.mod(paladin.stats.charisma))

        paladin.weapon.hitBonus shouldBe charismaBonus
      }
    }

    "make the Paladins weapon have a hit bonus equal to its Charisma modifier (minimum of 1) plus the weapons base hit bonus" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val paladin = random[Paladin]
          .withNoFightingStyles()
          .withBaseWeapon(PlusOneShortsword)
          .withCondition(SacredWeaponCondition())

        val charismaBonus = Math.max(1, Modifier.mod(paladin.stats.charisma))

        paladin.weapon.hitBonus shouldBe charismaBonus + PlusOneShortsword.hitBonus
      }
    }

    "reset the Paladins weapon when the condition is removed" in {
      fail("TODO")
    }

    "reset the Paladins magical weapon when the condition is removed" in {
      fail("TODO")
    }
  }

  "Sacred Weapon" should {
    "apply the SacredWeaponCondition to Paladin" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val levelThreePaladin = random[Paladin].withLevel(LevelThree).withCombatIndex(1)

        val (Combatant(_, updatedPaladin: Paladin), _) =
          sacredWeapon(1)(levelThreePaladin).useAbility(List.empty[Combatant], LowestFirst)

        updatedPaladin.conditions shouldBe List(SacredWeaponCondition())
      }
    }

    "set the Paladins channelDivinityUsed to true" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val levelThreePaladin = random[Paladin].withLevel(LevelThree).withCombatIndex(1)

        val updatedPaladin = sacredWeapon(1)(levelThreePaladin).update.asInstanceOf[Paladin]

        updatedPaladin.channelDivinityUsed shouldBe true
      }
    }

    "meet the condition if channel divinity has not been used" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val paladin = random[Paladin].withCombatIndex(1)

        sacredWeapon(1)(paladin).conditionMet shouldBe true
      }
    }

    "not meet the condition if channel divinity has already been used" in {
      new TestContext {
        override implicit val roll: RollStrategy = Dice.defaultRandomiser

        val paladin = random[Paladin].withChannelDivinityUsed().withCombatIndex(1)

        sacredWeapon(1)(paladin).conditionMet shouldBe false
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
