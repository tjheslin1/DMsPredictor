package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarianAbilities._
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class BaseBarbarianAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "rage" should {

    "update the barbarian's number of rages left" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val updatedBarbarian = rage(Priority)(ragedBarbarian).update.asInstanceOf[BaseBarbarian]

      updatedBarbarian.rageUsages shouldBe 1
    }

    "update the barbarian's inRage to true" in new TestContext {
      val ragedBarbarian = random[Barbarian].withRageUsagesLeft(2).withCombatIndex(1)

      val updatedBarbarian = rage(Priority)(ragedBarbarian).update.asInstanceOf[BaseBarbarian]

      updatedBarbarian.inRage shouldBe true
    }

    "reset the rage turns count back to 10" in new TestContext {
      val barbarian = random[Barbarian]
        .withRageUsagesLeft(2)
        .withRageTurnsLeft(5)
        .withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.rageTurnsLeft shouldBe 10
    }

    "add resistance to Bludgeoning, Piercing and Slashing damage" in new TestContext {
      val barbarian = random[Barbarian].withNoResistancesOrImmunities().withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.resistances shouldBe List(Bludgeoning, Piercing, Slashing)
    }

    "use the Barbarian's bonus action" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val ragingBarbarian = rage(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      ragingBarbarian.bonusActionUsed shouldBe true
    }
  }

  "Reckless Attack" should {

    "set the Barbarian's attackStatus to Advantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val recklessBarbarian = recklessAttack(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      recklessBarbarian.attackStatus shouldBe Advantage
    }

    "set the Barbarian's defenseStatus to Disadvantage" in new TestContext {
      val barbarian = random[Barbarian].withCombatIndex(1)

      val recklessBarbarian = recklessAttack(Priority)(barbarian).update.asInstanceOf[BaseBarbarian]

      recklessBarbarian.defenseStatus shouldBe Disadvantage
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
