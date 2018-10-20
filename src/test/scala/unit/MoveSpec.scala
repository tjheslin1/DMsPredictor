package unit

import base.PropertyChecksBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.model.Move._
import io.github.tjheslin1.model.{Creature, Dice, Monster, PlayerCharacter}
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Queue

class MoveSpec extends WordSpec with Matchers with PropertyChecksBase {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player = c1.copy(creatureType = PlayerCharacter)
        val enemy  = c2.copy(creatureType = Monster)

        val queue = Queue(player, enemy)

        takeMove(queue).map(_.name) shouldBe Queue(enemy.name, player.name)
      }
    }

    "update head enemy after attack" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player = c1.copy(creatureType = PlayerCharacter)
        val enemy  = c2.copy(creatureType = Monster)

        val queue = Queue(player, enemy)

        val List(updatedEnemy, _) = takeMove(queue)(Dice.naturalTwenty).toList

        updatedEnemy.health should (be <= enemy.health)
      }
    }

    "focus mob with lowest health first" in {
      forAll { (c1: Creature, c2: Creature, c3: Creature) =>
        val player   = c1.copy(creatureType = PlayerCharacter, stats = c1.stats.copy(strength = 10))
        val enemyOne = c2.copy(creatureType = Monster, health = 1)
        val enemyTwo = c3.copy(creatureType = Monster, health = 50)

        val queue = Queue(player, enemyOne, enemyTwo)

        val result = takeMove(queue)(Dice.naturalTwenty)

        result.find(_.name == enemyOne.name).get.health should (be <= 0)
        result.find(_.name == enemyTwo.name).get.health shouldBe 50
      }
    }
  }
}
