package unit.barbarian

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.barbarian.TotemWarrior
import io.github.tjheslin1.dmspredictor.classes.barbarian.TotemWarrior.Bear
import io.github.tjheslin1.dmspredictor.model._

class TotemWarriorSpec extends UnitSpecBase {

  "standardTotemWarriorAbilities" should {
    "return all damage types except Psychic" in {
      TotemWarrior.rageResistances(Bear) shouldBe DamageType.allDamageTypes.diff(List(Psychic))
    }
  }
}
