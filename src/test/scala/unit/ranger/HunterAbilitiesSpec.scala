package unit.ranger

import base.{Tracking, UnitSpecBase}
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRangerAbilities.twoWeaponFighting
import io.github.tjheslin1.dmspredictor.classes.ranger.Hunter
import io.github.tjheslin1.dmspredictor.classes.ranger.HunterAbilities._
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model.Move.takeMove
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

import scala.collection.immutable.Queue

class HunterAbilitiesSpec extends UnitSpecBase {

  "Colossus Slayer" should {
    "be triggered if the enemy is below max health" in {
      forAll { (hunter: Hunter, goblin: Goblin) =>
        implicit val roll: RollStrategy = _ => RollResult(10)

        val hunterCombatant = hunter.withCombatIndex(1)

        val goblinCombatant = goblin
          .withHealth(45)
          .withMaxHealth(50)
          .withCombatIndex(2)

        colossusSlayer(1)(hunterCombatant).triggerMet(List(goblinCombatant)) shouldBe true
      }
    }

    "not be triggered if the enemy is at max health" in {
      forAll { (hunter: Hunter, goblin: Goblin) =>
        implicit val roll: RollStrategy = _ => RollResult(10)

        val hunterCombatant = hunter.withCombatIndex(1)

        val goblinCombatant = goblin
          .withHealth(50)
          .withMaxHealth(50)
          .withCombatIndex(2)

        colossusSlayer(1)(hunterCombatant).triggerMet(List(goblinCombatant)) shouldBe false
      }
    }

    "set colossusSlayerUsed to true" in {
      val hunter = random[Hunter]
        .withColossusSlayerUsed(false)
        .withAbilities(List(colossusSlayer(1)))
        .withCombatIndex(1)

      val updatedHunter = colossusSlayer(1)(hunter).update.asInstanceOf[Hunter]

      updatedHunter.colossusSlayerUsed shouldBe true
    }

    "only be used once per turn even if the Hunter has multiple attacks" in {
      forAll { (hunter: Hunter, testMonster: TestMonster) =>
        implicit val roll: RollStrategy = _ => RollResult(10)

        val twoWeaponFightingHunter = hunter
          .withAbilities(List(colossusSlayer(1), extraAttack(2), twoWeaponFighting(3)))
          .withLevel(LevelThree)
          .withStrength(10)
          .withDexterity(10)
          .withBaseWeapon(Shortsword)
          .withOffHand(Shortsword)
          .withCombatIndex(1)

        val injuredGoblin = testMonster
          .withArmourClass(2)
          .withHealth(45)
          .withMaxHealth(50)
          .withCombatIndex(2)

        val Queue(Combatant(_, updatedTestMonster: TestMonster), _) =
          takeMove(Queue(twoWeaponFightingHunter, injuredGoblin), LowestFirst)

        val damageOfBothWeapons  = 10 * 2
        val colossusSlayerDamage = 10

        updatedTestMonster.health shouldBe 45 - damageOfBothWeapons - colossusSlayerDamage
      }
    }
  }

  abstract private class TestContext extends Tracking {
    implicit val roll: RollStrategy
  }
}
