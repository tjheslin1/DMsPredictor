package unit.strategy

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.Focus._
import io.github.tjheslin1.dmspredictor.strategy.{LowestFirst, PlayerHealing}
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

class FocusSpec extends UnitSpecBase with OptionValues {

  "nextToFocus" should {

    "focus mob with lowest health first" in new TestContext {
      forAll {
        (fighter: Fighter,
         monsterOne: TestMonster,
         monsterTwo: TestMonster,
         monsterThree: TestMonster) =>
          val enemyOne   = monsterOne.withHealth(50).withCombatIndex(2)
          val enemyTwo   = monsterTwo.withHealth(1).withCombatIndex(3)
          val enemyThree = monsterThree.withHealth(50).withCombatIndex(4)

          val enemies = List(enemyOne, enemyTwo, enemyThree)

          nextToFocus(fighter.withCombatIndex(1), enemies, LowestFirst).value shouldBe enemyTwo
      }
    }

    "ignore player which is a Rogue hidden from the monster" in new TestContext {
      forAll { (goblin: Goblin, fighter: Fighter, cleric: Cleric, rogue: Rogue) =>
        val blindGoblin = goblin.withCombatIndex(1)
        val playerOne   = fighter.withHealth(50).withCombatIndex(2)
        val playerTwo   = cleric.withHealth(20).withCombatIndex(3)
        val playerThree = rogue.isHiddenFrom(List(blindGoblin)).withHealth(10).withCombatIndex(4)

        val allies = List(playerOne, playerTwo, playerThree)

        nextToFocus(blindGoblin, allies, LowestFirst).value shouldBe playerTwo
      }
    }

    "focus player to heal with lowest health first" in new TestContext {
      forAll { (goblin: Goblin, fighter: Fighter, cleric: Cleric, barbarian: Barbarian) =>
        val playerOne   = fighter.withHealth(50).withCombatIndex(2)
        val playerTwo   = cleric.withHealth(1).withCombatIndex(3)
        val playerThree = barbarian.withHealth(0).withCombatIndex(4)

        val allies = List(playerOne, playerTwo, playerThree)

        nextToFocus(goblin.withCombatIndex(1), allies, PlayerHealing).value shouldBe playerThree
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
