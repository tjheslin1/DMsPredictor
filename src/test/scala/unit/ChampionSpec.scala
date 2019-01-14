package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.model._

class ChampionSpec extends UnitSpecBase {

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
}
