package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class CoreAbilitiesSpec extends UnitSpecBase {

  "Extra Attack" should {
    "make two weapon attacks" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (testMonster: TestMonster, fighter: Fighter) =>
        var swordUsedCount = 0
        val trackedSword = Weapon("sword", Melee, Slashing, twoHands = false, {
          swordUsedCount += 1
          1
        })

        val swordedMonster = testMonster
          .withWeapon(trackedSword)
          .withStrength(10)
          .withAbilities(List(1 -> CoreAbilities.extraAttack))
          .withCombatIndex(1)

        Move.takeMove(Queue(swordedMonster, fighter.withDexterity(10).withCombatIndex(2)), LowestFirst)

        swordUsedCount shouldBe 2
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
