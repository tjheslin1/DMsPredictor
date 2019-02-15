package unit.fighter

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class ChampionAbilitiesSpec extends UnitSpecBase {

  "Champion" should {

    import Champion._

    "make 2 attacks using Action Surge to make two Attack actions" in {

      forAll { (champion: Champion, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val swordFighter = _abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(champion)
            .withLevel(LevelTwo)
            .withBaseWeapon(trackedSword)
            .withCombatIndex(1)

          val monster = testMonster.withArmourClass(5).withCombatIndex(2)

          Move.takeMove(Queue(swordFighter, monster), LowestFirst)

          swordUsedCount shouldBe 2
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var swordUsedCount = 0
    val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
      swordUsedCount += 1
      1
    })
  }
}
