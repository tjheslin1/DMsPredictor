package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.Dice
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue

class MoveSpec extends UnitSpecBase {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.creature.withCombatIndex(1), monster.testMonster.withCombatIndex(2))

        takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.testMonster.name, fighter.creature.name)
      }
    }

    "update head enemy after attack" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.creature.withCombatIndex(1), monster.testMonster.withCombatIndex(2))

        val Queue(updatedEnemy, _) = takeMove(queue, LowestFirst)(Dice.naturalTwenty)

        updatedEnemy.creature.health should (be <= monster.testMonster.health)
      }
    }

    "ignore unconscious mobs" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>
        val player   = fighter.creature.withStrength(10).withCombatIndex(1)
        val enemyOne = monsterOne.testMonster.withHealth(0).withCombatIndex(2)
        val enemyTwo = monsterTwo.testMonster.withHealth(1).withCombatIndex(3)

        val queue = Queue(player, enemyOne, enemyTwo)

        val Queue(_, updatedEnemyTwo, _) = takeMove(queue, LowestFirst)(Dice.naturalTwenty)

        updatedEnemyTwo.creature.health shouldBe 0
      }
    }

    "focus mob with lowest health first" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster, monsterThree: TestMonster) =>

        val player     = fighter.creature.withStrength(10).withCombatIndex(1)
        val enemyOne   = monsterOne.testMonster.withHealth(50).withCombatIndex(2)
        val enemyTwo   = monsterTwo.testMonster.withHealth(1).withCombatIndex(3)
        val enemyThree = monsterThree.testMonster.withHealth(50).withCombatIndex(4)

        val queue = Queue(player, enemyOne, enemyTwo, enemyThree)

        val Queue(updatedEnemyOne, updatedEnemyTwo, updatedEnemyThree, _) =
          takeMove(queue, LowestFirst)(Dice.naturalTwenty)

        updatedEnemyOne.creature.health shouldBe 50
        updatedEnemyTwo.creature.health shouldBe 0
        updatedEnemyThree.creature.health shouldBe 50
      }
    }
  }
}
