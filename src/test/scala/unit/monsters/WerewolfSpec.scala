package unit.monsters

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Werewolf
import util.TestData._

class WerewolfSpec extends UnitSpecBase {

  "set the Werewolf to dead if the damage brings health below negative max health" in new TestContext {
    implicit override val roll: RollStrategy = _ => RollResult(10)

    val werewolf = random[Werewolf]
      .withHealth(50)
      .withMaxHealth(50)

    val updatedWerewolf = werewolf.updateHealth(110, Fire, Hit).asInstanceOf[Werewolf]

    updatedWerewolf.isAlive shouldBe false
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
