package unit.ranger

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.equipment.weapons.Longbow
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import util.TestMonster
import util.TestData._

class RangerSpellsSpec extends UnitSpecBase {

  "HuntersMark" should {
    "apply an extra 1d6 damage to the casters weapon" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(4)

          val levelTwoRanger = ranger
              .withAllSpellSlotsAvailableForLevel(LevelTwo)
              .withLevel(LevelTwo)
              .withDexterity(10) // no added damage to bow
              .withBaseWeapon(Longbow)
              .asInstanceOf[Ranger]

          val monster = testMonster.withCombatIndex(2)

          levelTwoRanger.weapon.damage shouldBe 4

          val (updatedRanger: Ranger, _) =
            HuntersMark.effect(levelTwoRanger, 1, List(monster))

          updatedRanger.conditions shouldBe List(HuntersMarkCondition)
          updatedRanger.weapon.damage shouldBe 8 // normal bow damage + hunters mark damage
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
