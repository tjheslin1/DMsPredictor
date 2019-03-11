package unit.fighter

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.BattleMasterAbilities.disarmingAttackManeuver
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.{Focus, LowestFirst}
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._

import scala.collection.immutable.Queue

class BattleMasterAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "Disarming Attack Maneuver" should {

    import BattleMaster._

    "spend available superiority dice to use disarming attack" in {
      val battleMasterCombatant = random[BattleMaster]
        .withSuperiorityDiceCount(2)
        .withLevel(LevelThree)
        .withCombatIndex(1)

      val updatedBattleMaster: BattleMaster =
        disarmingAttackManeuver(Priority)(battleMasterCombatant).update.asInstanceOf[BattleMaster]

      updatedBattleMaster.superiorityDiceCount shouldBe 1
    }

    "disarm opponent permanently using Disarming Attack" in {
      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val battleMasterCombatant = battleMaster
            .withSuperiorityDiceCount(1)
            .withLevel(LevelThree)
            .withStrength(20)
            .withCombatIndex(1)

          val monster = goblin.withArmourClass(5).withStrength(1).withCombatIndex(2)

          val (_, List(Combatant(_, updatedMonster: Creature))) =
            disarmingAttackManeuver(Priority)(battleMasterCombatant)
              .useAbility(List(monster), LowestFirst)

          updatedMonster.baseWeapon.name shouldBe UnarmedStrike(updatedMonster).name
        }
      }
    }

    "use all available superiority dice during turn" in {
      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val battleMasterCombatant = _abilityUsages
            .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(battleMaster)
            .withSuperiorityDiceCount(4)
            .withLevel(LevelFive)
            .withNoOffHand()
            .withStrength(20)
            .withCombatIndex(1)

          val monster = goblin.withArmourClass(5).withHealth(1000).withStrength(1).withCombatIndex(2)

          val Queue(_, Combatant(_, updatedBattleMaster: BattleMaster)) =
            Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

          updatedBattleMaster.superiorityDiceCount shouldBe 0
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    var trackedAbilityUsedCount = 0
    var trackedAbilityUsed      = false
    def trackedAbility(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String     = "test-tracked-ability-one"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = trackedAbilityUsed == false

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        trackedAbilityUsedCount += 1
        (combatant, others)
      }

      def update: Creature = {
        trackedAbilityUsed = true
        combatant.creature
      }
    }
  }
}
