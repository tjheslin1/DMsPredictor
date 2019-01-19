package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class BattleMasterAbilitiesSpec extends UnitSpecBase {

  "BattleMaster" should {

    import BattleMaster._

    "spend available superiority dice to use disarming attack" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (battleMaster: BattleMaster, testMonster: TestMonster) =>

        val battleMasterCombatant = _abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = true))(battleMaster)
            .withSuperiorityDiceCount(1)
          .withLevel(LevelThree)
          .withCombatIndex(1)

        val monster = testMonster.withArmourClass(5).withCombatIndex(2)

        val Queue(_, Combatant(_, updatedBattleMaster: BattleMaster)) =
          Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

        updatedBattleMaster.superiorityDiceCount shouldBe 0
      }
    }

    "disarm opponent permanently using Disarming Attack" in new TestContext {
      override implicit val roll: RollStrategy = D20.naturalTwenty

      forAll { (battleMaster: BattleMaster, testMonster: TestMonster) =>

        val battleMasterCombatant = _abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = true))(battleMaster)
          .withSuperiorityDiceCount(1)
          .withLevel(LevelThree)
          .withCombatIndex(1)

        val Queue(Combatant(_, updatedMonster: Creature), _) =
          Move.takeMove(Queue(battleMasterCombatant, testMonster.withCombatIndex(2)), LowestFirst)

        updatedMonster.baseWeapon shouldBe UnarmedStrike
      }
    }

    "use all four superiority dice when using Action Surge with Extra Attack" in new TestContext {
      fail("todo")
    }

    "use all available superiority dice during turn" in new TestContext {
      fail("todo")
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
