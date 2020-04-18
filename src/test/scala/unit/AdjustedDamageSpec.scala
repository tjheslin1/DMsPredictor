package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class AdjustedDamageSpec extends UnitSpecBase {

  "adjustedDamage" should {

    "deal full damage to a creature not resistance or immune to the damage type" in {
      forAll { monster: TestMonster =>
        val monsterCombatant = monster.withNoResistancesOrImmunities().withHealth(100)

        adjustedDamage(10, Slashing, monsterCombatant) shouldBe 10
      }
    }

    "deal half damage to a creature resistance to the damage type" in {
      forAll { monster: TestMonster =>
        val resistantMonster = monster.withDamageResistance(Slashing).withHealth(100)

        adjustedDamage(10, Slashing, resistantMonster) shouldBe 5
      }
    }

    "deal half damage rounded down to a creature resistance to the damage type" in {
      forAll { monster: TestMonster =>
        val resistantMonster = monster.withDamageResistance(Slashing).withHealth(100)

        adjustedDamage(11, Slashing, resistantMonster) shouldBe 5
      }
    }

    "deal no damage to a creature immune to the damage type" in {
      forAll { monster: TestMonster =>
        val immuneMonster = monster.withDamageImmunity(Slashing).withHealth(100)

        adjustedDamage(10, Slashing, immuneMonster) shouldBe 0
      }
    }
  }
}
