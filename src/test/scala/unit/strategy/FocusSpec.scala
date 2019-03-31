package unit.strategy

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Focus._
import io.github.tjheslin1.dmspredictor.strategy.{LowestFirst, PlayerHealing}
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

class FocusSpec extends UnitSpecBase with OptionValues {

  "nextToFocus" should {

    "focus mob with lowest health first" in new TestContext {
    forAll { (monsterOne: TestMonster, monsterTwo: TestMonster, monsterThree: TestMonster) =>
    val enemyOne   = monsterOne.withHealth(50).withCombatIndex(1)
    val enemyTwo   = monsterTwo.withHealth(1).withCombatIndex(2)
    val enemyThree = monsterThree.withHealth(50).withCombatIndex(3)

    val enemies = List(enemyOne, enemyTwo, enemyThree)

    nextToFocus(enemies, LowestFirst).value shouldBe enemyTwo
  }
    }

    "focus ally with lowest health first" in new TestContext {
      forAll { (fighter: Fighter, cleric: Cleric, barbarian: Barbarian) =>
        val allyOne   = fighter.withHealth(50).withCombatIndex(2)
        val allyTwo   = cleric.withHealth(1).withCombatIndex(3)
        val allyThree = barbarian.withHealth(0).withCombatIndex(4)

        val allies = List(allyOne, allyTwo, allyThree)

        nextToFocus(allies, PlayerHealing).value shouldBe allyThree
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
