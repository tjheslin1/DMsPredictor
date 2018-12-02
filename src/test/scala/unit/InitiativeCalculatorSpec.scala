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
        val player = fighter.creature.withName("fighter").withCombatIndex(1)
        val goblin = monsterOne.creature.withName("goblin").withCombatIndex(2)
        val dragon = monsterTwo.creature.withName("dragon").withCombatIndex(3)

        val initiative = Map(player.index -> Initiative(player, 15),
                             goblin.index -> Initiative(goblin, 10),
                             dragon.index -> Initiative(dragon, 20))

        val woundedFighter = player.withHealth(2)
        val woundedGoblin  = goblin.withHealth(1)
        val woundedDragon  = dragon.withHealth(5)

        val pcs  = List(woundedFighter)
        val mobs = List(woundedGoblin, woundedDragon)

        updateInitiative(initiative, pcs, mobs) shouldBe Map(
          player.index -> Initiative(woundedFighter, 15),
          dragon.index -> Initiative(woundedDragon, 20),
          goblin.index -> Initiative(woundedGoblin, 10)
        )
      }
    }
  }
}
