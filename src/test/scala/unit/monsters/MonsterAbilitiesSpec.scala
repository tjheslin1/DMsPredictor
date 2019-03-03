package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, SingleAttack, WholeAction}
import io.github.tjheslin1.dmspredictor.monsters.MonsterAbilities._
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import util.TestData._
import util.TestMonster

class MonsterAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "multiAttack" should {
    "attack a number of times equal to the total provided" in {
      val totalNumberOfAttacks = 2

      forAll { (testMonster: TestMonster, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val monsterCombatant =
            testMonster.withStrength(20).withBaseWeapon(trackedSword).withCombatIndex(1)
          val healthFighter = fighter.withHealth(100).withDexterity(1).withNoArmour()

          val (_, List(Combatant(_, updatedFighter: Fighter))) = multiAttack(Priority, totalNumberOfAttacks)(monsterCombatant)
            .useAbility(List(healthFighter.withCombatIndex(2)), LowestFirst)

          swordUsedCount shouldBe 2
          updatedFighter.health shouldBe healthFighter.health - 12 // wpn damage (1) + strength (5) * 2
        }
      }
    }

    "attack a high number of times equal to the total provided" in {
      val totalNumberOfAttacks = 5

      forAll { (testMonster: TestMonster, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val monsterCombatant =
            testMonster.withStrength(20).withBaseWeapon(trackedSword).withCombatIndex(1)
          val fighterCombatant = fighter.withHealth(200).withDexterity(1).withNoArmour().withCombatIndex(2)

          multiAttack(Priority, totalNumberOfAttacks)(monsterCombatant)
            .useAbility(List(fighterCombatant), LowestFirst)

          swordUsedCount shouldBe 5
        }
      }
    }

    "delegate to a SingleAttack ability" in {
      val totalNumberOfAttacks = 2

      forAll { (testMonster: TestMonster, fighter: Fighter) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val monsterCombatant = testMonster
            .withStrength(20)
            .withBaseWeapon(trackedSword)
            .withAbilities(
              List(multiAttack(Priority, totalNumberOfAttacks),
                   trackedActionAbility(2),
                   singleUseAttackAbility(3)))
            .withCombatIndex(1)

          val fighterCombatant = fighter.withDexterity(1).withNoArmour().withCombatIndex(2)

          multiAttack(Priority, totalNumberOfAttacks)(monsterCombatant)
            .useAbility(List(fighterCombatant), LowestFirst)

          swordUsedCount shouldBe 1
          trackedActionAbilityUsed shouldBe false
          singleUseAttackAbilityUsed shouldBe true
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    var swordUsedCount = 0
    val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      swordUsedCount += 1
      1
    })

    var singleUseAttackAbilityUsed = false
    def singleUseAttackAbility(currentOrder: Int)(combatant: Combatant): Ability =
      new Ability(combatant) {
        val name: String     = "test-tracked-ability-single-use"
        val order            = currentOrder
        val levelRequirement = LevelOne
        val abilityAction    = SingleAttack

        def triggerMet(others: List[Combatant]) = true
        def conditionMet: Boolean               = singleUseAttackAbilityUsed == false

        def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) =
          (combatant, others)

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
  }
}
