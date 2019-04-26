package unit

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class MoveSpec extends UnitSpecBase with OptionValues {

  val Priority = 1

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

          takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.name,
                                                                           fighter.name)
        }
      }
    }

    "replace unconscious creature to back of queue after attacking" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue = Queue(fighter.withHealth(0).withCombatIndex(1), monster.withCombatIndex(2))

          takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.name,
                                                                           fighter.name)
        }
      }
    }

    "use a players bonus action ability if unused after main action" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val bonusActionFighter =
            fighter.withAbilities(List(trackedBonusAction(1))).withCombatIndex(1)

          val queue = Queue(bonusActionFighter, monster.withCombatIndex(2))

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

          updatedFighter.bonusActionUsed shouldBe true
          trackedBonusActionUsed shouldBe true
        }
      }
    }

    "reset player's bonus action to unused" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue =
            Queue(fighter.withBonusActionUsed().withCombatIndex(1), monster.withCombatIndex(2))

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

          updatedFighter.bonusActionUsed shouldBe false
        }
      }
    }

    "reset player's reaction to unused" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue =
            Queue(fighter.withReactionUsed().withCombatIndex(1), monster.withCombatIndex(2))

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

          updatedFighter.reactionUsed shouldBe false
        }
      }
    }

    "call a creature's resetTurn at the beginning of their move" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          var turnReset = false
          val monster   = testMonster.withResetTurn(_ => turnReset = true)

          val queue =
            Queue(monster.withCombatIndex(1), fighter.withCombatIndex(2))

          takeMove(queue, LowestFirst)

          turnReset shouldBe true
        }
      }
    }

    "reset unconscious creatures bonus action to unused" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue = Queue(fighter.withBonusActionUsed().withHealth(0).withCombatIndex(1),
                            monster.withCombatIndex(2))

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

          updatedFighter.bonusActionUsed shouldBe false
        }
      }
    }

    "update head enemy after attack" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

          val Queue(Combatant(_, updatedEnemy), _) = takeMove(queue, LowestFirst)(D20.naturalTwenty)

          updatedEnemy.health should (be <= monster.health)
        }
      }
    }

    "ignore unconscious mobs" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val player = fighter.withAllAbilitiesUsed().withStrength(20).withCombatIndex(1)

          val enemyOne = monsterOne.withHealth(0).withCombatIndex(2)
          val enemyTwo = monsterTwo.withArmourClass(1).withHealth(1).withCombatIndex(3)

          val queue = Queue(player, enemyOne, enemyTwo)

          val Queue(_, Combatant(_, updatedEnemyTwo), _) =
            takeMove(queue, LowestFirst)(D20.naturalTwenty)

          updatedEnemyTwo.health shouldBe 0
        }
      }
    }

    "call Ability" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          val trackedFighter =
            fighter.withAbilities(List(trackedAbility(Priority))).withCombatIndex(1)

          takeMove(Queue(trackedFighter, monster.withCombatIndex(2)), LowestFirst)

          trackedAbilityUsedCount shouldBe 1
          trackedAbilityUsed shouldBe true
        }
      }
    }

    "handle conditions" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll = _ => RollResult(10)

          val trackedGoblin =
            goblin
              .withConditions(trackedStartOfTurnCondition(100),
                              trackedStartOfTurnCondition(50),
                              trackedEndOfTurnCondition(100))
              .withCombatIndex(1)
          val fighterCombatant = fighter.withCombatIndex(2)

          takeMove(Queue(trackedGoblin, fighterCombatant), LowestFirst)

          trackedStartOfTurnConditionHandledCount shouldBe 2
          trackedEndOfTurnConditionHandledCount shouldBe 1
        }
      }
    }

    "miss the combatants turn if a condition saving throw is failed" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val trackedGoblin =
            goblin
              .withBaseWeapon(trackedSword)
              .withConditions(trackedStartOfTurnCondition(100, turnMissed = true))
              .withCombatIndex(1)
          val fighterCombatant = fighter.withCombatIndex(2)

          takeMove(Queue(trackedGoblin, fighterCombatant), LowestFirst)

          trackedStartOfTurnConditionHandledCount shouldBe 1
          swordUsedCount shouldBe 0
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
