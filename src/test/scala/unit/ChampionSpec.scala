package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.fighter.Champion
import io.github.tjheslin1.dmspredictor.model._

class ChampionSpec extends UnitSpecBase {

  "improvedCritical" should {
    "return true for a CriticalHit for a roll of 19 or 20 for level three or above" in {
      forAll { champion: Champion =>
        val levelThreeChampion = champion.copy(level = LevelThree)
        val levelFiveChampion = champion.copy(level = LevelFive)

        Champion.improvedCritical.attackIsCritical(levelThreeChampion, 19) shouldBe true
        Champion.improvedCritical.attackIsCritical(levelThreeChampion, 20) shouldBe true

        Champion.improvedCritical.attackIsCritical(levelFiveChampion, 19) shouldBe true
        Champion.improvedCritical.attackIsCritical(levelFiveChampion, 20) shouldBe true
      }
    }
  }
}
