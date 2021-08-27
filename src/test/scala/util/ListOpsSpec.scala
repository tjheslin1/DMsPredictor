package util

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.Barbarian
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.condition.Turned
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import util.TestData._

class ListOpsSpec extends UnitSpecBase {

  import io.github.tjheslin1.dmspredictor.util.ListOps._

  "replace" should {
    "add a new element if it doesn't already exist" in {
      val numbers = List(1, 2, 3, 4)

      numbers.replace(5) shouldBe List(1, 2, 3, 4, 5)
    }

    "replace element in single element list" in {
      val numbers = List(4)

      numbers.replace(4) shouldBe List(4)
    }

    "replace an existing element" in {
      val numbers = List(1, 2, 3, 4)

      numbers.replace(3) shouldBe List(1, 2, 3, 4)
    }

    "add new elements if they don't already exist" in {
      val numbers = List(1, 2, 3, 4)

      numbers.replace(List(5, 6, 7)) shouldBe List(1, 2, 3, 4, 5, 6, 7)
    }

    "add a mix of new and existing elements if they don't already exist" in {
      val numbers = List(1, 2, 3, 4)

      numbers.replace(List(4, 5, 6)) shouldBe List(1, 2, 3, 4, 5, 6)
    }

    "replace existing elements" in {
      val numbers = List(1, 2, 3, 4)

      numbers.replace(List(3, 4)) shouldBe List(1, 2, 3, 4)
    }

    "add a new product element if it doesn't already exist" in {
      val cleric    = random[Cleric].withCombatIndex(1)
      val barbarian = random[Barbarian].withCombatIndex(2)
      val goblin    = random[Goblin].withCombatIndex(3)
      val zombie    = random[Zombie].withCombatIndex(4)

      val creatures = List(cleric, barbarian, goblin)

      creatures.replace(zombie) shouldBe List(cleric, barbarian, goblin, zombie)
    }

    "replace existing product element" in {
      val cleric    = random[Cleric].withCombatIndex(1)
      val barbarian = random[Barbarian].withCombatIndex(2)
      val goblin    = random[Goblin].withCombatIndex(3)
      val zombie    = random[Zombie].withCombatIndex(4)

      val creatures = List(cleric, barbarian, goblin, zombie)

      creatures.replace(zombie) shouldBe List(cleric, barbarian, goblin, zombie)
    }

    "add a mix of new and existing product elements if they don't already exist" in {
      val cleric    = random[Cleric].withCombatIndex(1)
      val barbarian = random[Barbarian].withCombatIndex(2)
      val goblin    = random[Goblin].withCombatIndex(3)
      val zombie    = random[Zombie].withCombatIndex(4)
      val fighter   = random[Fighter].withCombatIndex(5)

      val creatures = List(cleric, barbarian, goblin, zombie)

      creatures
        .replace(List(zombie, fighter)) shouldBe List(cleric, barbarian, goblin, zombie, fighter)
    }

    "replace product element in single product element list" in {
      val cleric        = random[Cleric]
      val updatedCleric = cleric.withCondition(Turned(10, 10))

      List(cleric.withCombatIndex(1)).replace(updatedCleric.withCombatIndex(1)) shouldBe List(
        updatedCleric.withCombatIndex(1))
    }
  }
}
