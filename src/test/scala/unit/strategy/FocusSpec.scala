package unit.strategy

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

class FocusSpec extends UnitSpecBase with OptionValues {

  "nextToFocus" should {

    "focus mob with lowest health first" in new TestContext {
      forAll { (monsterOne: TestMonster, monsterTwo: TestMonster, monsterThree: TestMonster) =>
        val enemyOne   = monsterOne.withHealth(50).withCombatIndex(2)
        val enemyTwo   = monsterTwo.withHealth(1).withCombatIndex(3)
        val enemyThree = monsterThree.withHealth(50).withCombatIndex(4)

        val enemies = List(enemyOne, enemyTwo, enemyThree)

        nextToFocus(enemies, LowestFirst).value shouldBe enemyTwo
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
