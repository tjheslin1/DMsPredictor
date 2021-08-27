package unit.strategy

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.Focus._
import io.github.tjheslin1.dmspredictor.strategy.{Healing, LowestFirst}
import org.scalatest.OptionValues
import util.TestData._
import util.TestMonster

class FocusSpec extends UnitSpecBase with OptionValues {

  "nextToFocus" should {

    "focus mob with lowest health first" in {
      forAll {
        (
            fighter: Fighter,
            monsterOne: TestMonster,
            monsterTwo: TestMonster,
            monsterThree: TestMonster
        ) =>
          new TestContext {
            implicit val roll: RollStrategy = Dice.defaultRandomiser

            val enemyOne   = monsterOne.withHealth(50).withCombatIndex(1)
            val enemyTwo   = monsterTwo.withHealth(1).withCombatIndex(2)
            val enemyThree = monsterThree.withHealth(50).withCombatIndex(3)

            val enemies = List(enemyOne, enemyTwo, enemyThree)

            nextToFocus(fighter.withCombatIndex(1), enemies, LowestFirst).value shouldBe enemyTwo
          }
      }
    }

    "ignore player which is a Rogue hidden from the monster" in {
      forAll { (goblin: Goblin, fighter: Fighter, cleric: Cleric, rogue: Rogue) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val blindGoblin = goblin.withCombatIndex(1)
          val playerOne   = fighter.withHealth(50).withCombatIndex(2)
          val playerTwo   = cleric.withHealth(20).withCombatIndex(3)
          val playerThree = rogue.isHiddenFrom(List(blindGoblin)).withHealth(10).withCombatIndex(4)

          val allies = List(playerOne, playerTwo, playerThree)

          nextToFocus(blindGoblin, allies, LowestFirst).value shouldBe playerTwo
        }
      }
    }

    "focus player to heal with lowest health first" in {
      forAll { (cleric: Cleric, fighter: Fighter, wizard: Wizard, barbarian: Barbarian) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val playerOne   = fighter.withHealth(50).withMaxHealth(50).withCombatIndex(1)
          val playerTwo   = wizard.withHealth(1).withMaxHealth(50).withCombatIndex(2)
          val playerThree = barbarian.withHealth(0).withMaxHealth(50).withCombatIndex(3)

          val allies = List(playerOne, playerTwo, playerThree)

          nextToFocus(cleric.withCombatIndex(1), allies, Healing).value shouldBe playerThree
        }
      }
    }

    "focus monster to heal with lowest health first" in {
      forAll { (lich: Lich, goblin: Goblin, werewolf: Werewolf, zombie: Zombie) =>
        new TestContext {
          implicit val roll: RollStrategy = Dice.defaultRandomiser

          val healerLich = lich.withCombatIndex(1)

          val monsterOne   = goblin.withHealth(50).withMaxHealth(50).withCombatIndex(3)
          val monsterTwo   = werewolf.withHealth(1).withMaxHealth(50).withCombatIndex(3)
          val monsterThree = zombie.withHealth(0).withMaxHealth(50).withCombatIndex(4)

          val allies = List(monsterOne, monsterTwo, monsterThree)

          nextToFocus(healerLich, allies, Healing).value shouldBe monsterThree
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
