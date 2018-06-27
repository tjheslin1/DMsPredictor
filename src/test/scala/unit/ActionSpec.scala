package unit

import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model._
import io.github.tjheslin1.monsters.Goblin
import org.scalatest.{Matchers, WordSpec}

class ActionSpec extends WordSpec with Matchers {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      val pc      = Fighter.levelOneFighter().creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.attack(pc, monster)(_ => 20) shouldBe CriticalHit
    }

    "hit a monster if the attack overcomes the monsters armour class" in {
      val pc      = Fighter.levelOneFighter().creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.attack(pc, monster)(_ => 19) shouldBe Hit
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      val pc      = Fighter.levelOneFighter().creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.attack(pc, monster)(_ => 2) shouldBe Miss
    }

    "miss if the attack roll was a natural 1" in {
      val pc      = Fighter.levelOneFighter().creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.attack(pc, monster)(_ => 1) shouldBe CriticalMiss
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      val pc      = Fighter.levelOneFighter().creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.resolveDamage(pc, monster, Hit)(_ => 100) shouldBe (pc, monster.copy(health = 0))
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      val zeroDamageWeapon = new Weapon() {
        def damage(implicit rollStrategy: RollStrategy): Int = 0
      }

      val pc      = Fighter.levelOneFighter(zeroDamageWeapon).creature
      val monster = Goblin.levelOneGoblin().creature

      Actions.resolveDamage(pc, monster, Hit)(_ => 12) shouldBe (pc, monster)
    }
  }
}
