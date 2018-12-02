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
        val player = fighter.withName("fighter")
        val goblin = monsterOne.withName("goblin")
        val dragon = monsterTwo.withName("dragon")

        val initiative = Map("fighter" -> Initiative(player.creature, 15),
                             "goblin" -> Initiative(goblin.creature, 10),
                             "dragon" -> Initiative(dragon.creature, 20))

        val woundedFighter = player.withHealth(2).creature
        val woundedGoblin  = goblin.withHealth(1).creature
        val woundedDragon  = dragon.withHealth(5).creature

        val pcs  = List(woundedFighter)
        val mobs = List(woundedGoblin, woundedDragon)

        updateInitiative(initiative, pcs, mobs) shouldBe Map(
          "fighter" -> Initiative(woundedFighter, 15),
          "dragon"  -> Initiative(woundedDragon, 20),
          "goblin"  -> Initiative(woundedGoblin, 10)
        )
      }
    }
  }
}
