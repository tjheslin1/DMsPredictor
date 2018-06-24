package unit

import org.scalatest.{Matchers, WordSpec}
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.{RollResult, RollStrategy, Weapon}
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{Loss, Success}
import io.github.tjheslin1.weapons.Shortsword

class PlayerCharacterSpec extends WordSpec with Matchers {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit a monster if the attack overcomes the monsters armour class" in {
      val pc      = Fighter()
      val monster = Goblin()

      pc.attack(monster)(_ => 20) shouldBe Success
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      val pc      = Fighter()
      val monster = Goblin()

      pc.attack(monster)(_ => 0) shouldBe Loss
    }
  }

  // TODO
//  "resolveDamage" should {
//    "kill a monster if the damage is more than the monsters health" in {
//      val pc      = Fighter()
//      val monster = Goblin()
//
//      pc.resolveDamage(Shortsword, monster)(_ => 100) shouldBe Success
//    }
//
//    "fail to kill a monster if the damage is less than the monsters health" in {
//      val zeroDamageWeapon = new Weapon() {
//        def damage(implicit rollStrategy: RollStrategy): Int = 0
//      }
//
//      val pc      = Fighter(zeroDamageWeapon)
//      val monster = Goblin()
//
//      pc.resolveDamage(pc.weapon, monster)(_ => 12) shouldBe Loss
//    }
//  }
}
