package unit

import io.github.tjheslin1.model.Dice
import io.github.tjheslin1.model.Move._
import org.scalatest.{Matchers, WordSpec}
import util.TestCreature

import scala.collection.immutable.Queue

class MoveSpec extends WordSpec with Matchers {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      val creature = TestCreature.player
      val enemy    = TestCreature.enemy

      val queue = Queue(creature, enemy)

      takeMove(queue).map(_.name) shouldBe Queue(enemy.name, creature.name)
    }

    "update head enemy after attack" in {
      val creature = TestCreature.player
      val enemy    = TestCreature.enemy

      val queue = Queue(creature, enemy)

      takeMove(queue)(Dice.naturalTwenty) shouldBe Queue(enemy.copy(health = 0), creature)
    }
  }
}
