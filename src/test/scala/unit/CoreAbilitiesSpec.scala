package unit

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{EldritchKnight, Fighter}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

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
            .useAbility(monster.some)

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
              List(extraAttack(Priority), trackedActionAbility(2), trackedAttackAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          extraAttack(Priority)(trackedAbilityFighter)
            .useAbility(testMonster.withArmourClass(5).withCombatIndex(2).some)

          trackedAttackUsedCount shouldBe 2
          trackedActionAbilityUsedCount shouldBe 0
        }
      }
    }

    "delegate to an ability lower in order then default to an attack" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val trackedAbilityFighter = fighter
            .withAbilities(
              List(extraAttack(Priority), trackedActionAbility(2), singleUseAttackAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster: Combatant = testMonster.withArmourClass(5).withCombatIndex(2)

          extraAttack(Priority)(trackedAbilityFighter).useAbility(monster.some)

          trackedAttackUsedCount shouldBe 1
          trackedActionAbilityUsedCount shouldBe 0
        }
      }
    }
  }

  "Cast Spell" should {

    "cast a spell (spell attack) using the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val eldritchKnightCombatant = eldritchKnight
            .withSpellKnown(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          castOffensiveSpell(Priority)(eldritchKnightCombatant).useAbility(monster.some)

          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "cast a spell (saving throw) using the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val spellCastingEK = eldritchKnight
            .withSpellKnown(trackedSavingThrowSpell)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()

          val eldritchKnightCombatant = spellCastingEK
            .withLevel(LevelThree)
            .withIntelligence(10)
            .withProficiencyBonus(6)
            .withCombatIndex(1)

          val monster = testMonster.withWisdom(10).withCombatIndex(2)

          val Queue(_, Combatant(_, updatedEK: EldritchKnight)) =
            Move.takeMove(Queue(eldritchKnightCombatant, monster), LowestFirst)

          savingThrowSpellUsedCount shouldBe 1
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val spellCastingEK = eldritchKnight
            .withSpellKnown(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()

          val eldritchKnightCombatant = spellCastingEK.withLevel(LevelThree).withCombatIndex(1)

          val monster = testMonster.withArmourClass(10).withCombatIndex(2)

          val updatedEldritchKnight: EldritchKnight =
            castOffensiveSpell(Priority)(eldritchKnightCombatant).update.asInstanceOf[EldritchKnight]

          updatedEldritchKnight.spellSlots.firstLevel.count shouldBe (spellCastingEK.spellSlots.firstLevel.count - 1)
        }
      }
    }

    "deal half damage to a creature resistant to the damage type" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val eldritchKnightCombatant = eldritchKnight
            .withSpellKnown(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster
            .withHealth(10)
            .withResistance(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, Some(Combatant(_, updatedMonster: TestMonster))) =
            castOffensiveSpell(Priority)(eldritchKnightCombatant).useAbility(monster.some)

          updatedMonster.health shouldBe 8
        }
      }
    }

    "deal zero damage to a creature immune to the damage type" in {
      forAll { (eldritchKnight: EldritchKnight, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val eldritchKnightCombatant = eldritchKnight
            .withSpellKnown(trackedMeleeSpellAttack)
            .withAllBaseFighterAbilitiesUsed()
            .withAllSpellSlotsAvailable()
            .withLevel(LevelThree)
            .withCombatIndex(1)

          val monster = testMonster
            .withHealth(10)
            .withImmunity(Fire)
            .withArmourClass(10)
            .withCombatIndex(2)

          val (_, Some(Combatant(_, updatedMonster: TestMonster))) =
            castOffensiveSpell(Priority)(eldritchKnightCombatant).useAbility(monster.some)

          updatedMonster.health shouldBe 10
        }
      }
    }

    "cast cantrip if defined and no spell slots are available" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val noSpellSlotsCleric = cleric
            .withCantrip(trackedMeleeSpellAttack)
            .withSpellKnown(trackedSavingThrowSpell)
            .withNoSpellSlotsAvailable()
            .withWisdom(24)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(2).withCombatIndex(2)

          castOffensiveSpell(Priority)(noSpellSlotsCleric).useAbility(monster.some)

          savingThrowSpellUsedCount shouldBe 0
          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric]
        .withNoCantrip()
        .withSpellKnown(trackedSavingThrowSpell)
        .withNoSpellSlotsAvailable()
        .withWisdom(24)
        .withCombatIndex(1)

      castOffensiveSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "must meet the level requirement to use spellcasting" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val levelTwoEldritchKnight = random[EldritchKnight]
        .withSpellKnown(trackedSavingThrowSpell)
        .withNoSpellSlotsAvailable()
        .withLevel(LevelTwo)
        .withCombatIndex(1)

      castOffensiveSpell(Priority)(levelTwoEldritchKnight).conditionMet shouldBe false
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var swordUsedCount = 0
    val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      swordUsedCount += 1
      1
    })

    var trackedAttackUsedCount = 0
    def trackedAttackAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-single-attack"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = SingleAttack

        def triggerMet(target: Option[Combatant]) = true
        def conditionMet: Boolean                 = true

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, target)
        }

        def update: Creature =
          combatant.creature
      }

    var singleUseAttackAbilityUsed = false
    def singleUseAttackAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-single-use"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = SingleAttack

        def triggerMet(target: Option[Combatant]) = true
        def conditionMet: Boolean                 = singleUseAttackAbilityUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          singleUseAttackAbilityUsed = true
          combatant.creature
        }
      }

    var trackedActionAbilityUsedCount = 0
    var trackedActionAbilityUsed      = false
    def trackedActionAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-action"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = WholeAction

        def triggerMet(target: Option[Combatant]) = true
        def conditionMet: Boolean                 = trackedActionAbilityUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedActionAbilityUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedActionAbilityUsed = true
          combatant.creature
        }
      }

    var meleeSpellUsedCount = 0
    val trackedMeleeSpellAttack =
      Spell("tracked-melee-spell-test", 1, Evocation, OneAction, MeleeSpellAttack, Fire, {
        meleeSpellUsedCount += 1
        4
      })

    var savingThrowSpellUsedCount = 0
    val trackedSavingThrowSpell = Spell("tracked-saving-throw-spell-test",
                                        1,
                                        Evocation,
                                        OneAction,
                                        SpellSavingThrow(Wisdom),
                                        Fire, {
                                          savingThrowSpellUsedCount += 1
                                          4
                                        })
  }
}
