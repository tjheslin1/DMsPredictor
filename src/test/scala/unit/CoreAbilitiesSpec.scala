package unit

import base.UnitSpecBase
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
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
            .useAbility(monster.some)

          swordUsedCount shouldBe 2
        }
      }
    }

    "set the Player's Bonus Action used to true" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val updatedPlayer =
            extraAttack(Priority)(fighter.withCombatIndex(1)).update.asInstanceOf[Player]

          updatedPlayer.bonusActionUsed shouldBe true
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

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = true

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

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = singleUseAttackAbilityUsed == false

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

        val triggerMet: Boolean   = true
        def conditionMet: Boolean = trackedActionAbilityUsed == false

        def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
          trackedActionAbilityUsedCount += 1
          (combatant, target)
        }

        def update: Creature = {
          trackedActionAbilityUsed = true
          combatant.creature
        }
      }
  }
}
