package unit.monsters

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.SingleAttack
import io.github.tjheslin1.dmspredictor.monsters.MonsterAbilities._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
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

          val (_, List(Combatant(_, updatedFighter: Fighter))) =
            multiAttack(Priority, totalNumberOfAttacks)(monsterCombatant)
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
          val fighterCombatant =
            fighter.withHealth(200).withDexterity(1).withNoArmour().withCombatIndex(2)

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

          val singleAttackAbility: CombatantAbility =
            trackedAbility(3,
                           action = SingleAttack,
                           condition = singleUseAttackAbilityUsed == false,
                           updatedTracking = singleUseAttackAbilityUsed = true)

          val monsterCombatant = testMonster
            .withStrength(20)
            .withBaseWeapon(trackedSword)
            .withAbilities(
              List(multiAttack(Priority, totalNumberOfAttacks),
                   trackedAbility(2),
                   singleAttackAbility))
            .withCombatIndex(1)

          val fighterCombatant = fighter.withDexterity(1).withNoArmour().withCombatIndex(2)

          multiAttack(Priority, totalNumberOfAttacks)(monsterCombatant)
            .useAbility(List(fighterCombatant), LowestFirst)

          swordUsedCount shouldBe 1
          trackedAbilityUsed shouldBe false
          singleUseAttackAbilityUsed shouldBe true
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy

    var singleUseAttackAbilityUsed = false
  }
}
