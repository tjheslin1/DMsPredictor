package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.BattleMasterAbilities.disarmingAttackManeuver
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import io.github.tjheslin1.dmspredictor.util.IntOps._
import util.TestData._

import scala.collection.immutable.Queue

class BattleMasterAbilitiesSpec extends UnitSpecBase {

  "BattleMaster" should {

    import BattleMaster._

    "spend available superiority dice to use disarming attack" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(19)

      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        val battleMasterCombatant = battleMaster
          .withAllAbilitiesUsed()
          .withSuperiorityDiceCount(2)
          .withLevel(LevelThree)
          .withCombatIndex(1)

        val monster = goblin.withStrength(1).withArmourClass(5).withCombatIndex(2)

        val Queue(_, Combatant(_, updatedBattleMaster: BattleMaster)) =
          Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

        updatedBattleMaster.superiorityDiceCount shouldBe 1
      }
    }

    "disarm opponent permanently using Disarming Attack" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        val battleMasterCombatant = battleMaster
          .withAllAbilitiesUsed()
          .withSuperiorityDiceCount(1)
          .withLevel(LevelThree)
          .withStrength(20)
          .withCombatIndex(1)

        val monster = goblin.withArmourClass(5).withStrength(1).withCombatIndex(2)

        val Queue(Combatant(_, updatedMonster: Creature), _) =
          Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

        updatedMonster.baseWeapon.name shouldBe UnarmedStrike(updatedMonster).name
      }
    }

    "use all four superiority dice when using Action Surge with Extra Attack" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        val battleMasterCombatant = _abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(battleMaster)
          .withSuperiorityDiceCount(4)
          .withLevel(LevelFive)
          .withStrength(20)
          .withCombatIndex(1)

        val monster = goblin.withArmourClass(5).withStrength(1).withCombatIndex(2)

        val Queue(_, Combatant(_, updatedBattleMaster: BattleMaster)) =
          Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

        updatedBattleMaster.superiorityDiceCount shouldBe 0
      }
    }

    "use all available superiority dice during turn" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      forAll { (battleMaster: BattleMaster, goblin: Goblin) =>
        val battleMasterCombatant = _abilityUsages
          .set(BaseFighterAbilities(secondWindUsed = true, actionSurgeUsed = false))(battleMaster)
          .withSuperiorityDiceCount(3)
          .withLevel(LevelFive)
          .withNoOffHand()
          .withStrength(20)
          .withCombatIndex(1)

        val monster = goblin.withArmourClass(5).withStrength(1).withCombatIndex(2)

        val Queue(_, Combatant(_, updatedBattleMaster: BattleMaster)) =
          Move.takeMove(Queue(battleMasterCombatant, monster), LowestFirst)

        updatedBattleMaster.superiorityDiceCount shouldBe 0
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

      val triggerMet: Boolean   = true
      def conditionMet: Boolean = trackedAbilityUsed == false

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
        trackedAbilityUsedCount += 1
        (combatant, target)
      }

      def update: Creature = {
        trackedAbilityUsed = true
        combatant.creature
      }
    }
  }
}
