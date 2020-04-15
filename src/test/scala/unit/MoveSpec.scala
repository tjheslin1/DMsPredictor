package unit

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.model.{condition, _}
import io.github.tjheslin1.dmspredictor.model.condition.{Paralyzed, Turned}
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
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
            Queue(fighter.withReactionUsed(true).withCombatIndex(1), monster.withCombatIndex(2))

          val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

          updatedFighter.reactionUsed shouldBe false
        }
      }
    }

    "call a creature's resetTurn at the beginning of their move" in {
      forAll { barbarian: Barbarian =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.defaultRandomiser

          var turnReset = false
          val queue =
            Queue(barbarian.withAttackStatus(Advantage).withCombatIndex(1))

          val Queue(Combatant(_, updatedBarbarian: Barbarian)) = takeMove(queue, LowestFirst)

          updatedBarbarian.attackStatus shouldBe Regular
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

          val player = fighter.withAllAbilitiesUsed().withStrength(20).withDexterity(20).withCombatIndex(1)

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

    "remove condition at start of turn if turnsLeft is 0" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val turnedCondition = Turned(saveDc = 1, turnsLeft = 0)
          val turnedFighter   = fighter.withCondition(turnedCondition).withCombatIndex(1)

          val Queue(Combatant(_, updatedFighter: Fighter)) =
            takeMove(Queue(turnedFighter), LowestFirst)

          updatedFighter.conditions shouldBe List()
        }
      }
    }

    "remove condition at end of turn if turnsLeft is 0" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val paralyzedCondition = Paralyzed(saveDc = 1, turnsLeft = 0, Constitution)
          val paralyzedFighter   = fighter.withCondition(paralyzedCondition).withCombatIndex(1)

          val Queue(Combatant(_, updatedFighter: Fighter)) =
            takeMove(Queue(paralyzedFighter), LowestFirst)

          updatedFighter.conditions shouldBe List()
        }
      }
    }

    "decrement turnsLeft on condition at start of turn" in {
      forAll { fighter: Fighter =>
        new TestContext {
          implicit override val roll = D20.naturalTwenty

          val paralyzedTurnedFighter =
            fighter.withConditions(trackedStartOfTurnCondition(dc = 1)).withCombatIndex(1)

            takeMove(Queue(paralyzedTurnedFighter), LowestFirst)

          trackedStartOfTurnConditionDecremented shouldBe true
        }
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
