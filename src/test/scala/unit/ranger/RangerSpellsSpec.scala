package unit.ranger

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.equipment.weapons.Longbow
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
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

          updatedRanger.conditions shouldBe List(HuntersMarkBuffCondition)
          updatedRanger.weapon.damage shouldBe 8 // normal bow damage + hunters mark damage
        }
      }
    }

    "be removed if the casters loses concentration" in {
      forAll { ranger: Ranger =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(4)

          val levelTwoRanger = ranger
            .withAllSpellSlotsAvailableForLevel(LevelTwo)
            .withLevel(LevelTwo)
            .withDexterity(10) // no added damage to bow
            .withConstitution(4) // to save concentration check
            .withHealth(50)
            .withMaxHealth(50)
            .withBaseWeapon(Longbow)
            .asInstanceOf[Ranger]

          levelTwoRanger.weapon.damage shouldBe 4

          val (updatedRanger: Ranger, _) =
            HuntersMark.effect(levelTwoRanger, 1, List.empty[Combatant])

          updatedRanger.conditions shouldBe List(HuntersMarkBuffCondition)
          updatedRanger.weapon.damage shouldBe 8 // normal bow damage + hunters mark damage

          val healthUpdatedRanger = updatedRanger.updateHealth(20, Bludgeoning, Hit)

          healthUpdatedRanger.conditions shouldBe List.empty[Condition]
          healthUpdatedRanger.weapon.damage shouldBe 4 // normal bow damage only
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
