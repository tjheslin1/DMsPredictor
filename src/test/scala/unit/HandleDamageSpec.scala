package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model.HandleDamage._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class HandleDamageSpec extends UnitSpecBase {

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

  "applyDamage" should {
    "update the targets health" in {
      val cleric = random[Cleric].withHealth(50).withMaxHealth(50)

      val updatedCleric = applyDamage(cleric, 7).asInstanceOf[Cleric]

      updatedCleric.health shouldBe 50 - 7
    }

    "set the Cleric to dead if the damage takes it below max health" in {
      val cleric = random[Cleric].withHealth(50).withMaxHealth(50)

      val updatedCleric = applyDamage(cleric, 110).asInstanceOf[Cleric]

      updatedCleric.health shouldBe 0
      updatedCleric.isAlive shouldBe false
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
