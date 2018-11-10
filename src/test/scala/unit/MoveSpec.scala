package unit

import base.PropertyChecksBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.Dice
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Queue

class MoveSpec extends WordSpec with Matchers with PropertyChecksBase {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>

        val queue = Queue(fighter.creature, monster.creature)

        takeMove(queue, LowestFirst).map(_.name) shouldBe Queue(monster.creature.name, fighter.creature.name)
      }
    }

    "update head enemy after attack" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>

        val queue = Queue(fighter.creature, monster.creature)

        val List(updatedEnemy, _) = takeMove(queue, LowestFirst)(Dice.naturalTwenty).toList

        updatedEnemy.health should (be <= monster.creature.health)
      }
    }

    "ignore unconscious mobs" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>

        val player   = fighter.creature.copy(stats = fighter.creature.stats.copy(strength = 10))
        val enemyOne = monsterOne.creature.copy(health = 0)
        val enemyTwo = monsterTwo.creature.copy(health = 1)

        val queue = Queue(player, enemyOne, enemyTwo)

        val result = takeMove(queue, LowestFirst)(Dice.naturalTwenty)

        result.find(_.name == enemyOne.name).get.health shouldBe 0
        result.find(_.name == enemyTwo.name).get.health shouldBe 0
      }
    }

    "focus mob with lowest health first" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster, monsterThree: TestMonster) =>

        val player   = fighter.creature.copy(stats = fighter.creature.stats.copy(strength = 10))
        val enemyOne = monsterOne.creature.copy(health = 50)
        val enemyTwo = monsterTwo.creature.copy(health = 1)
        val enemyThree = monsterThree.creature.copy(health = 50)

        val queue = Queue(player, enemyOne, enemyTwo, enemyThree)

        val result = takeMove(queue, LowestFirst)(Dice.naturalTwenty)

        result.find(_.name == enemyOne.name).get.health shouldBe 50
        result.find(_.name == enemyTwo.name).get.health shouldBe 0
        result.find(_.name == enemyThree.name).get.health shouldBe 50
      }
    }
  }
}
