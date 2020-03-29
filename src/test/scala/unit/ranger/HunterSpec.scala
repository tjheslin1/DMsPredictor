package unit.ranger

import base.UnitSpecBase
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.{Defense, Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import util.TestData._

class HunterSpec extends UnitSpecBase {

  "updateHealth" should {
    "not handle concentration if damage taken was 0" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingHunter = random[Hunter]
        .withSpellKnown(HuntersMark)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)

      val updatedHunter = concentratingHunter.updateHealth(0, Bludgeoning, Hit).asInstanceOf[Hunter]

      updatedHunter.concentratingSpell shouldBe HuntersMark.some
    }

    "handle loss of concentration if ranger goes unconscious" in new TestContext {
      implicit val roll: RollStrategy = _ => RollResult(19)

      val concentratingHunter = random[Hunter]
        .withSpellKnown(HuntersMark)
        .withAllSpellSlotsAvailableForLevel(LevelTwo)
        .withConcentratingOn(HuntersMark)
        .withLevel(LevelTwo)
        .withConstitution(2)
        .withHealth(1)
        .withMaxHealth(50)

      val updatedRanger = concentratingHunter.updateHealth(1, Bludgeoning, Hit).asInstanceOf[Hunter]

      updatedRanger.concentratingSpell shouldBe none[Spell]
    }
  }

  "weapon" should {
    "roll extra damage for HuntersMark spell if active" in {
      forAll { hunter: Hunter =>
        new TestContext {
          implicit val roll: RollStrategy = _ => RollResult(6)

          val unbuffedHunter = hunter
            .withFightingStyle(Defense)
            .withBaseWeapon(Shortsword)
            .withStrength(10)
            .withDexterity(10)

          val buffedHunter = hunter
            .withFightingStyle(Defense)
            .withConcentratingOn(HuntersMark)
            .withCondition(HuntersMarkBuffCondition)
            .withBaseWeapon(Shortsword)
            .withStrength(10)
            .withDexterity(10)

          unbuffedHunter.weapon.damage shouldBe 6

          buffedHunter.weapon.damage shouldBe 6 + 6 // Shortsword 1 * D6 plus HuntersMarkDamage 1 * D6
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
