package unit.monsters

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.monsters.Vampire
import util.TestData._

class VampireSpec extends UnitSpecBase {

  "resetStartOfTurn" should {
    "regenerate 20 hit points if the Vampire hasn't taken Radiant damage last turn" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire = vampire.withHealth(50).withMaxHealth(100).resetStartOfTurn()

        regeneratedVampire.health shouldBe 70
      }
    }

    "not regenerate hit points if the Vampire has taken Radiant damage last turn" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire =
          vampire.copy(radiantDamageTaken = true).withHealth(50).withMaxHealth(100).resetStartOfTurn()

        regeneratedVampire.health shouldBe 50
      }
    }

    "reset radiantDamageTaken back to false" in {
      forAll { vampire: Vampire =>
        val regeneratedVampire = vampire
          .copy(radiantDamageTaken = true)
          .resetStartOfTurn()
          .asInstanceOf[Vampire]

        regeneratedVampire.radiantDamageTaken shouldBe false
      }
    }
  }
}
