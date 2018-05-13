package unit

import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}

class CreatureSpec extends WordSpec with Matchers {

  "generate" should {
    "create an instance of a creature, rolling its health" in {
      val creature = new TestCreature(BaseStats(8, 14, 10, 10, 8, 8))
    }
  }

  case class TestCreature(sts: BaseStats) extends Creature {

    val stats = sts
    val health = D6*2
    val armourClass = 15
    val experience = 50
  }
}
