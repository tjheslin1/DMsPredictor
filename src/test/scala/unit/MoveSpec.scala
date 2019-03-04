package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class MoveSpec extends UnitSpecBase with OptionValues {

  val Priority = 1

  "takeMove" should {
    "replace creature to back of queue after attacking" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

        takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.name, fighter.name)
      }
    }

    "replace unconscious creature to back of queue after attacking" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withHealth(0).withCombatIndex(1), monster.withCombatIndex(2))

        takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.name, fighter.name)
      }
    }

    "reset player's bonus action to unused" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue =
          Queue(fighter.withBonusActionUsed().withHealth(0).withCombatIndex(1), monster.withCombatIndex(2))

        val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

        updatedFighter.bonusActionUsed shouldBe false
      }
    }

    "call a creature's resetTurn at the beginning of their move" in new TestContext {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        var turnReset = false
        val monster   = testMonster.withResetTurn(_ => turnReset = true)

        val queue =
          Queue(monster.withCombatIndex(1), fighter.withCombatIndex(2))

        takeMove(queue, LowestFirst)

        turnReset shouldBe true
      }
    }

    "reset unconscious creatures bonus action to unused" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withBonusActionUsed().withHealth(0).withCombatIndex(1),
                          monster.withCombatIndex(2))

        val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

        updatedFighter.bonusActionUsed shouldBe false
      }
    }

    "update head enemy after attack" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

        val Queue(Combatant(_, updatedEnemy), _) = takeMove(queue, LowestFirst)(D20.naturalTwenty)

        updatedEnemy.health should (be <= monster.health)
      }
    }

    "ignore unconscious mobs" in new TestContext {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>
        val player = fighter.withAllAbilitiesUsed().withStrength(20).withCombatIndex(1)

        val enemyOne = monsterOne.withHealth(0).withCombatIndex(2)
        val enemyTwo = monsterTwo.withArmourClass(1).withHealth(1).withCombatIndex(3)

        val queue = Queue(player, enemyOne, enemyTwo)

        val Queue(_, Combatant(_, updatedEnemyTwo), _) =
          takeMove(queue, LowestFirst)(D20.naturalTwenty)

        updatedEnemyTwo.health shouldBe 0
      }
    }

    "call Ability" in new TestContext {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val trackedFighter =
          fighter.withAbilities(List(trackedAbility(Priority))).withCombatIndex(1)

        takeMove(Queue(trackedFighter, monster.withCombatIndex(2)), LowestFirst)

        trackedAbilityUsedCount shouldBe 1
        trackedAbilityUsed shouldBe true
      }
    }

    "handle conditions" in new TestContext {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll = _ => RollResult(10)

          val trackedGoblin =
            goblin.withConditions(trackedCondition(100), trackedCondition(50)).withCombatIndex(1)
          val fighterCombatant = fighter.withCombatIndex(2)

          takeMove(Queue(trackedGoblin, fighterCombatant), LowestFirst)

          trackedConditionHandledCount shouldBe 2
        }
      }
    }

    "miss the combatants turn if a condition saving throw is failed" in new TestContext {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val trackedGoblin =
            goblin.withBaseWeapon(trackedSword).withConditions(trackedCondition(100, turnMissed = true)).withCombatIndex(1)
          val fighterCombatant = fighter.withCombatIndex(2)

          takeMove(Queue(trackedGoblin, fighterCombatant), LowestFirst)

          trackedConditionHandledCount shouldBe 1
          swordUsedCount shouldBe 0
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

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false

    def trackedAbility(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String     = "test-tracked-ability-one"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = trackedAbilityUsed == false

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        trackedAbilityUsedCount += 1
        (combatant, others)
      }

      def update: Creature = {
        trackedAbilityUsed = true
        combatant.creature
      }
    }

    var trackedConditionHandledCount = 0
    def trackedCondition(dc: Int, turnMissed: Boolean = false): Condition = new Condition {
      val name = "tracked-condition"
      val missesTurn = turnMissed
      val handleOnDamage: Boolean = false

      val saveDc: Int    = dc
      val turnsLeft: Int = 10

      def handle[_: RS](creature: Creature): Creature = {
        trackedConditionHandledCount += 1
        creature
      }

      def handleOnDamage[_: RS](creature: Creature): Creature = creature
    }
  }
}
