package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.Initiative
import io.github.tjheslin1.dmspredictor.model.InitiativeCalculator.updateInitiative
import util.TestData._

class InitiativeCalculatorSpec extends UnitSpecBase {

  "updateInitiative" should {
    "update initiative with creatures updated health" in {
      forAll { (fighter: Fighter, monsterOne: TestMonster, monsterTwo: TestMonster) =>
        val player = fighter.withCombatIndex(1)
        val testMonsterOne = monsterOne.withCombatIndex(2)
        val testMonsterTwo = monsterTwo.withCombatIndex(3)

        val initiative = Map(player.index -> Initiative(player, 15),
                             testMonsterOne.index -> Initiative(testMonsterOne, 10),
                             testMonsterTwo.index -> Initiative(testMonsterTwo, 20))

        val woundedFighter = player.withCreature(fighter.withHealth(2))
        val woundedGoblin  = testMonsterOne.withCreature(monsterOne.withHealth(1))
        val woundedDragon  = testMonsterTwo.withCreature(monsterTwo.withHealth(5))

        val pcs  = List(woundedFighter)
        val mobs = List(woundedGoblin, woundedDragon)

        updateInitiative(initiative, pcs, mobs) shouldBe Map(
          player.index -> Initiative(woundedFighter, 15),
          testMonsterTwo.index -> Initiative(woundedDragon, 20),
          testMonsterOne.index -> Initiative(woundedGoblin, 10)
        )
      }
    }
  }
}
