package unit

import base.PropertyChecksBase
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.Move._
import io.github.tjheslin1.model.{Creature, Dice, Monster, PlayerCharacter, Weapon}
import org.scalatest.{Matchers, WordSpec}
import util.TestModel
import magnolia._
import scalacheckmagnolia.MagnoliaArbitrary._
import cats.syntax.show._
import cats.Show._
import io.github.tjheslin1.monsters.Goblin

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
      forAll { (player: Fighter, goblinOne: Goblin, goblinTwo: Goblin) =>

        val fighter  = player.creature.copy(weapon = Weapon("a", 5))
        val enemyOne = goblinOne.creature.copy(health = 5)
        val enemyTwo = goblinTwo.creature.copy(health = 50)

        println(player.show)
        println(enemyOne.show)

        val queue = Queue(fighter, enemyOne, enemyTwo)

        val result = takeMove(queue)(Dice.naturalTwenty)
        result.find(_.name == enemyOne.name).get.health shouldBe 0
        result.find(_.name == enemyTwo.name).get.health shouldBe 50

        player.creature.creatureType shouldBe PlayerCharacter
      }
    }
  }
}
