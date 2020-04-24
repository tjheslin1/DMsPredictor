package unit.fighter

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class ChampionSpec extends UnitSpecBase {

  "updateHealth" should {
    "set the Champion to dead if the damage brings health below negative max health" in new TestContext {
      override implicit val roll: RollStrategy = _ => RollResult(10)

      val champion = random[Champion]
        .withHealth(50)
        .withMaxHealth(50)

      val updatedChampion = champion.updateHealth(110, Bludgeoning, Hit).asInstanceOf[Champion]

      updatedChampion.isAlive shouldBe false
    }
  }

  "improvedCritical" should {
    "return true for a CriticalHit for a roll of 19 or 20 for level three or above" in {
      forAll { champion: Champion =>
        val levelOneChampion = champion.copy(level = LevelOne)
        val levelThreeChampion = champion.copy(level = LevelThree)
        val levelFiveChampion = champion.copy(level = LevelFive)

        levelOneChampion.scoresCritical(19) shouldBe false
        levelOneChampion.scoresCritical(20) shouldBe true

        levelThreeChampion.scoresCritical(19) shouldBe true
        levelThreeChampion.scoresCritical(20) shouldBe true

        levelFiveChampion.scoresCritical(19) shouldBe true
        levelFiveChampion.scoresCritical(20) shouldBe true
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
