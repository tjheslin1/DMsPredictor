package unit

import base.PropertyChecksBase
import io.github.tjheslin1.model.Move._
import io.github.tjheslin1.model.{Creature, Dice, Monster, PlayerCharacter, Strength}
import org.scalatest.{Matchers, WordSpec}
import util.TestModel

import scala.collection.immutable.Queue

class MoveSpec extends WordSpec with Matchers with PropertyChecksBase {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      val creature = TestModel.player
      val enemy    = TestModel.enemy

      val queue = Queue(creature, enemy)

      takeMove(queue).map(_.name) shouldBe Queue(enemy.name, creature.name)
    }

    "update head enemy after attack" in {
      val creature = TestModel.player
      val enemy    = TestModel.enemy

      val queue = Queue(creature, enemy)

      takeMove(queue)(Dice.naturalTwenty) shouldBe Queue(enemy.copy(health = 0), creature)
    }

    "focus mob with lowest health first" in {
      forAll { (c1: Creature, c2: Creature, c3: Creature) =>
        val player   = c1.copy(creatureType = PlayerCharacter, stats = c1.stats.copy(strength = Strength(10)))
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
