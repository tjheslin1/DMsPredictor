package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model._
import util.TestData.TestMonster

class ActionSpec extends UnitSpecBase {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        Actions.attack(fighter.creature, monster.creature)(_ => 20) shouldBe CriticalHit
      }
    }

    "hit a monster if the attack overcomes the monsters armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac10Monster = monster.creature.copy(armourClass = 10)

        Actions.attack(fighter.creature, ac10Monster)(_ => 19) shouldBe Hit
      }
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac20Monster = monster.creature.copy(armourClass = 20)

        Actions.attack(fighter.creature, ac20Monster)(_ => 2) shouldBe Miss
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        Actions.attack(fighter.creature, monster.creature)(_ => 1) shouldBe CriticalMiss
      }
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val oneHundredDamageWeapon = Weapon("one hundred damage weapon", 100)
        val player                 = fighter.creature.copy(weapon = oneHundredDamageWeapon)

        Actions.resolveDamage(player, monster.creature, Hit) shouldBe (player, monster.creature.copy(health = 0))
      }
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val zeroDamageWeapon = Weapon("zero damage weapon", 0)
        val player           = fighter.creature.copy(weapon = zeroDamageWeapon)

        Actions.resolveDamage(player, monster.creature, Hit)(_ => 12) shouldBe (player, monster.creature)
      }
    }
  }
}
