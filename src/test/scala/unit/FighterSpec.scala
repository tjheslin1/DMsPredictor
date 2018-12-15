package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.{Combatant, Dice, Move}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

import scala.collection.immutable.Queue

class FighterSpec extends UnitSpecBase {

  implicit val roll = Dice.defaultRandomiser

  "Fighter" should {
    "use Second Wind when it has reached a health threshold" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val lowHealthFighter = fighter.withHealth(1).withCombatIndex(1)
        val monster = testMonster.withCombatIndex(2)

        val Queue(_, Combatant(_, updatedFighter)) = Move.takeMove(Queue(lowHealthFighter, monster), LowestFirst)

        updatedFighter.health should be > 1
      }
    }
  }
}
