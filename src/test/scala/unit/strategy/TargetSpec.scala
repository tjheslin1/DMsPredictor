package unit.strategy

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Target
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

class TargetSpec extends UnitSpecBase with OptionValues {

    "monsters" should {

      "return list of all monsters" in new TestContext {
          val enemyOne   = random[TestMonster].withHealth(50).withCombatIndex(1)
          val enemyTwo   = random[TestMonster].withHealth(1).withCombatIndex(2)
          val enemyThree = random[TestMonster].withHealth(50).withCombatIndex(3)

        val playerOne = random[Fighter].withCombatIndex(4)
        val playerTwo = random[Cleric].withCombatIndex(5)

          val combatants = List(enemyOne, playerOne, enemyTwo, playerTwo, enemyThree)

          Target.monsters(combatants) should contain theSameElementsAs List(enemyOne, enemyTwo, enemyThree)
      }
    }

    "players" should {

      "return list of all players" in new TestContext {
          val enemyOne   = random[TestMonster].withHealth(50).withCombatIndex(1)
          val enemyTwo   = random[TestMonster].withHealth(1).withCombatIndex(2)
          val enemyThree = random[TestMonster].withHealth(50).withCombatIndex(3)

        val playerOne = random[Fighter].withCombatIndex(4)
        val playerTwo = random[Cleric].withCombatIndex(5)

          val combatants = List(enemyOne, playerOne, enemyTwo, playerTwo, enemyThree)

          Target.monsters(combatants) should contain theSameElementsAs List(playerOne, playerTwo)
      }
    }

    private class TestContext {
      implicit val roll: RollStrategy = Dice.defaultRandomiser
    }
  }
