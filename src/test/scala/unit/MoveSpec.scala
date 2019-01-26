package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.{BaseFighterAbilities, Fighter}
import io.github.tjheslin1.dmspredictor.model.Move._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class MoveSpec extends UnitSpecBase {

  implicit val roll = Dice.defaultRandomiser

  "takeMove" should {
    "replace creature to back of queue after attacking" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

        takeMove(queue, LowestFirst).map(_.creature.name) shouldBe Queue(monster.name, fighter.name)
      }
    }

    "reset player's bonus action to unused" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue =
          Queue(fighter.withBonusActionUsed().withCombatIndex(1), monster.withCombatIndex(2))

        val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

        updatedFighter.bonusActionUsed shouldBe false
      }
    }

    "reset unconscious creatures bonus action to unused" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withBonusActionUsed().withHealth(0).withCombatIndex(1), monster.withCombatIndex(2))

        val Queue(_, Combatant(_, updatedFighter: Fighter)) = takeMove(queue, LowestFirst)

        updatedFighter.bonusActionUsed shouldBe false
      }
    }

    "update head enemy after attack" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val queue = Queue(fighter.withCombatIndex(1), monster.withCombatIndex(2))

        val Queue(Combatant(_, updatedEnemy), _) = takeMove(queue, LowestFirst)(D20.naturalTwenty)

        updatedEnemy.health should (be <= monster.health)
      }
    }

    "ignore unconscious mobs" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>
        val player = Fighter._abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
          .withStrength(10)
          .withCombatIndex(1)

        val enemyOne = monsterOne.withHealth(0).withCombatIndex(2)
        val enemyTwo = monsterTwo.withHealth(1).withCombatIndex(3)

        val queue = Queue(player, enemyOne, enemyTwo)

        val Queue(_, Combatant(_, updatedEnemyTwo), _) = takeMove(queue, LowestFirst)(D20.naturalTwenty)

        updatedEnemyTwo.health shouldBe 0
      }
    }

    "focus mob with lowest health first" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster, monsterThree: TestMonster) =>
        val player = Fighter._abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(fighter)
          .withStrength(10)
          .withCombatIndex(1)

        val enemyOne   = monsterOne.withHealth(50).withCombatIndex(2)
        val enemyTwo   = monsterTwo.withHealth(1).withCombatIndex(3)
        val enemyThree = monsterThree.withHealth(50).withCombatIndex(4)

        val queue = Queue(player, enemyOne, enemyTwo, enemyThree)

        val Queue(Combatant(_, updatedEnemyOne), Combatant(_, updatedEnemyTwo), Combatant(_, updatedEnemyThree), _) =
          takeMove(queue, LowestFirst)(D20.naturalTwenty)

        updatedEnemyOne.health shouldBe 50
        updatedEnemyTwo.health shouldBe 0
        updatedEnemyThree.health shouldBe 50
      }
    }
  }
}
