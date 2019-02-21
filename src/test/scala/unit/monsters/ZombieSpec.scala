package unit.monsters

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Zombie
import util.TestData._

class ZombieSpec extends UnitSpecBase {

  "updatedHealth" should {
    "trigger Zombie's Undead Fortitude ability" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val lowHpZombie = zombie.withHealth(5).withConstitution(20).asInstanceOf[Zombie]

          val updatedZombie = lowHpZombie.updateHealth(10, Slashing, Hit)

          updatedZombie.health shouldBe 1
        }
      }
    }

    "trigger Zombie's Undead Fortitude ability with a DC of 5 plus damage taken" in {
      forAll { zombie: Zombie =>
        new TestContext {
          val lowHpZombie = zombie.withHealth(5).withConstitution(10).asInstanceOf[Zombie]

          val failedSaveZombie = lowHpZombie.updateHealth(10, Slashing, Hit)(_ => RollResult(14))
          failedSaveZombie.health shouldBe 0

          val passedSaveZombie = lowHpZombie.updateHealth(10, Slashing, Hit)(_ => RollResult(15))
          passedSaveZombie.health shouldBe 1
        }
      }
    }

    "keep Zombie unconscious if it fails it's Constitution saving throw" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = Dice.naturalOne

          val lowHpZombie = zombie.withHealth(5).withConstitution(1).asInstanceOf[Zombie]

          val updatedZombie = lowHpZombie.updateHealth(10, Slashing, Hit)

          updatedZombie.health shouldBe 0
        }
      }
    }

    "not trigger Undead Fortitude on a Critical Hit" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val lowHpZombie = zombie.withHealth(5).withConstitution(20).asInstanceOf[Zombie]

          val updatedZombie = lowHpZombie.updateHealth(10, Slashing, CriticalHit)

          updatedZombie.health shouldBe 0
        }
      }
    }

    "not trigger Undead Fortitude for Radiant damage" in {
      forAll { zombie: Zombie =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty
          val lowHpZombie                          = zombie.withHealth(5).withConstitution(20).asInstanceOf[Zombie]

          val updatedZombie = lowHpZombie.updateHealth(10, Radiant, Hit)

          updatedZombie.health shouldBe 0
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
