package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Zombie
import util.TestData._

class ZombieSpec extends UnitSpecBase {

  "resetStartOfTurn" should {
    "trigger Zombie's Undead Fortitude ability" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val unconsciousZombie = zombie.withHealth(0).withConstitution(20)

          val updatedZombie = unconsciousZombie.resetStartOfTurn()

          updatedZombie.health shouldBe 1
        }
      }
    }


    "keep Zombie unconscious if it fails it's Constitution saving throw" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.naturalOne

          val unconsciousZombie = zombie.withHealth(0).withConstitution(1)

          val updatedZombie = unconsciousZombie.resetStartOfTurn()

          updatedZombie.health shouldBe 0
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
