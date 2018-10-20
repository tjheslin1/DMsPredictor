package unit

import base.PropertyChecksBase
import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}

class ActionSpec extends WordSpec with Matchers with PropertyChecksBase {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player  = c1.copy(creatureType = PlayerCharacter)
        val monster = c2.copy(creatureType = Monster)

        Actions.attack(player, monster)(_ => 20) shouldBe CriticalHit
      }
    }

    "hit a monster if the attack overcomes the monsters armour class" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player  = c1.copy(creatureType = PlayerCharacter)
        val monster = c2.copy(creatureType = Monster, armourClass = 10)

        Actions.attack(player, monster)(_ => 19) shouldBe Hit
      }
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player  = c1.copy(creatureType = PlayerCharacter)
        val monster = c2.copy(creatureType = Monster, armourClass = 20)

        Actions.attack(player, monster)(_ => 2) shouldBe Miss
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (c1: Creature, c2: Creature) =>
        val player  = c1.copy(creatureType = PlayerCharacter)
        val monster = c2.copy(creatureType = Monster)

        Actions.attack(player, monster)(_ => 1) shouldBe CriticalMiss
      }
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      forAll { (c1: Creature, c2: Creature) =>
        val oneHundredDamageWeapon = Weapon("one hundred damage weapon", 100)

        val player  = c1.copy(creatureType = PlayerCharacter, weapon = oneHundredDamageWeapon)
        val monster = c2.copy(creatureType = Monster)

        Actions.resolveDamage(player, monster, Hit) shouldBe (player, monster.copy(health = 0))
      }
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      forAll { (c1: Creature, c2: Creature) =>
        val zeroDamageWeapon = Weapon("zero damage weapon", 0)

        val player  = c1.copy(creatureType = PlayerCharacter, weapon = zeroDamageWeapon)
        val monster = c2.copy(creatureType = Monster)

        Actions.resolveDamage(player, monster, Hit)(_ => 12) shouldBe (player, monster)
      }
    }
  }
}
