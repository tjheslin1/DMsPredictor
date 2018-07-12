package unit

import io.github.tjheslin1.model._
import org.scalatest.{Matchers, WordSpec}
import util.TestCreature

class ActionSpec extends WordSpec with Matchers {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      val player  = TestCreature.player
      val monster = TestCreature.enemy

      Actions.attack(player, monster)(_ => 20) shouldBe CriticalHit
    }

    "hit a monster if the attack overcomes the monsters armour class" in {
      val player  = TestCreature.player
      val monster = TestCreature.enemy

      Actions.attack(player, monster)(_ => 19) shouldBe Hit
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      val player  = TestCreature.player
      val monster = TestCreature.enemy

      Actions.attack(player, monster)(_ => 2) shouldBe Miss
    }

    "miss if the attack roll was a natural 1" in {
      val player  = TestCreature.player
      val monster = TestCreature.enemy

      Actions.attack(player, monster)(_ => 1) shouldBe CriticalMiss
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      val player  = TestCreature.player
      val monster = TestCreature.enemy

      Actions.resolveDamage(player, monster, Hit)(_ => 100) shouldBe (player, monster.copy(health = 0))
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      val zeroDamageWeapon = new Weapon() {
        def damage(implicit rollStrategy: RollStrategy): Int = 0
      }

      val player  = TestCreature.player.copy(weapon = zeroDamageWeapon)
      val monster = TestCreature.enemy

      Actions.resolveDamage(player, monster, Hit)(_ => 12) shouldBe (player, monster)
    }
  }
}
