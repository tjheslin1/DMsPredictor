package unit

import org.scalatest.{Matchers, WordSpec}
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.monsters.Goblin
import io.github.tjheslin1.simulation.{Loss, Success}

class PlayerCharacterSpec extends WordSpec with Matchers {

  "attack" should {
    "hit a monster if the attack overcomes the monsters armour class" in {
      val monster = Goblin()
      val pc      = Fighter()

      pc.attack(monster)(_ => 20) shouldBe Success
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      val monster = Goblin()
      val pc      = Fighter()

      pc.attack(monster)(_ => 0) shouldBe Loss
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      val monster = Goblin()
      val pc      = Fighter()

      pc.resolveDamage(monster)(_ => 100) shouldBe Success
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      val monster = Goblin()
      val pc      = Fighter()

      pc.resolveDamage(monster)(_ => 0) shouldBe Loss
    }
  }
}
