package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{EldritchKnight, Fighter}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.CureWounds
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.WizardSpells.MagicMissile
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
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
              List(extraAttack(Priority), trackedActionAbility(2), trackedAttackAbility(3)))
            .withLevel(LevelFive)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          extraAttack(Priority)(trackedAbilityFighter)
            .useAbility(List(monster), LowestFirst)

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

          extraAttack(Priority)(trackedAbilityFighter).useAbility(List(monster), LowestFirst)

          trackedAttackUsedCount shouldBe 1
          trackedActionAbilityUsedCount shouldBe 0
        }
      }
    }
  }

  "castSingleTargetOffensiveSpell" should {

    "cast a spell (spell attack)" in {
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

          castSingleTargetOffensiveSpell(Priority)(eldritchKnightCombatant)
            .useAbility(List(monster), LowestFirst)

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
            .withProficiencyBonus(6)
            .withLevel(LevelThree)
            .withIntelligence(10)
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
            castSingleTargetOffensiveSpell(Priority)(eldritchKnightCombatant).update
              .asInstanceOf[EldritchKnight]

          updatedEldritchKnight.spellSlots.firstLevel.count shouldBe (spellCastingEK.spellSlots.firstLevel.count - 1)
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

          castSingleTargetOffensiveSpell(Priority)(noSpellSlotsCleric).useAbility(List(monster),
                                                                                  LowestFirst)

          savingThrowSpellUsedCount shouldBe 0
          meleeSpellUsedCount shouldBe 1
        }
      }
    }

    "not meet the condition if the Spell Caster has only a healing spell to cast" in new TestContext {
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

    "must meet the level requirement to use spellcasting" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val levelTwoEldritchKnight = random[EldritchKnight].withLevel(LevelTwo).withCombatIndex(1)

      castSingleTargetOffensiveSpell(Priority)(levelTwoEldritchKnight).conditionMet shouldBe false
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

          val healingCleric  = cleric.withWisdom(12).withCombatIndex(1)
          val damagedFighter = fighter.withHealth(10).withMaxHealth(50).withCombatIndex(2)

          val (_, List(Combatant(_, healedFighter: Fighter))) =
            castSingleTargetHealingSpell(Priority)(healingCleric)
              .useAbility(List(damagedFighter), LowestFirst)

          healedFighter.creature.health shouldBe 21
        }
      }
    }

    "spend the highest available spell slot" in {
      forAll { cleric: Cleric =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(10)

          val healingCleric = cleric.withWisdom(12).asInstanceOf[Cleric]

          val updatedCleric =
            castSingleTargetHealingSpell(Priority)(healingCleric.withCombatIndex(1)).update
              .asInstanceOf[Cleric]

          updatedCleric.spellSlots.firstLevel.count shouldBe (healingCleric.spellSlots.firstLevel.count - 1)
        }
      }
    }

    "not meet the condition if the Spell Caster has only a damage spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withSpellKnown(MagicMissile).withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "not meet the condition if the Spell Caster has no spell to cast" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val cleric = random[Cleric].withNoCantrip().withNoSpellSlotsAvailable().withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(cleric).conditionMet shouldBe false
    }

    "must meet the level requirement to use spellcasting" in new TestContext {
      implicit override val roll: RollStrategy = _ => RollResult(10)

      val levelTwoEldritchKnight = random[EldritchKnight].withLevel(LevelTwo).withCombatIndex(1)

      castSingleTargetHealingSpell(Priority)(levelTwoEldritchKnight).conditionMet shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

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

        def triggerMet(others: List[Combatant]) = true
        def conditionMet: Boolean               = true

        def useAbility[_: RS](others: List[Combatant],
                              focus: Focus): (Combatant, List[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, others)
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

        def triggerMet(others: List[Combatant]) = true
        def conditionMet: Boolean               = singleUseAttackAbilityUsed == false

        def useAbility[_: RS](others: List[Combatant],
                              focus: Focus): (Combatant, List[Combatant]) = {
          trackedAttackUsedCount += 1
          (combatant, others)
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

        def triggerMet(others: List[Combatant]) = true
        def conditionMet: Boolean               = trackedActionAbilityUsed == false

        def useAbility[_: RS](others: List[Combatant],
                              focus: Focus): (Combatant, List[Combatant]) = {
          trackedActionAbilityUsedCount += 1
          (combatant, others)
        }

        def update: Creature = {
          trackedActionAbilityUsed = true
          combatant.creature
        }
      }

    var meleeSpellUsedCount = 0
    val trackedMeleeSpellAttack = new SingleTargetAttackSpell() {
      val damageType: DamageType   = Fire
      val name: String             = "tracked-fire-spell"
      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneAction
      val spellLevel: SpellLevel   = 1
      val concentration: Boolean   = false

      def damage[_: RS](spellCaster: SpellCaster): Int = {
        meleeSpellUsedCount += 1
        4
      }
    }

    var savingThrowSpellUsedCount = 0
    val trackedSavingThrowSpell: Spell = new SingleTargetSavingThrowSpell() {
      val attribute: Attribute      = Wisdom
      val halfDamageOnSave: Boolean = false
      val damageType: DamageType    = Fire
      val name: String              = "tracked-saving-throw-spell-test"
      val school: SchoolOfMagic     = Evocation
      val castingTime: CastingTime  = OneAction
      val spellLevel: SpellLevel    = 1
      val concentration: Boolean    = false

      def damage[_: RS](spellCaster: SpellCaster): Int = {
        savingThrowSpellUsedCount += 1
        4
      }
    }
  }
}
