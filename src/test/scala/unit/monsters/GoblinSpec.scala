package unit.monsters

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._

class GoblinSpec extends UnitSpecBase {

  "set the Goblin to dead if the damage brings health below negative max health" in new TestContext {
    implicit override val roll: RollStrategy = _ => RollResult(10)

    val werewolf = random[Goblin]
      .withHealth(50)
      .withMaxHealth(50)

    val updatedGoblin = werewolf.updateHealth(110, Fire, Hit).asInstanceOf[Goblin]

    updatedGoblin.isAlive shouldBe false
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
