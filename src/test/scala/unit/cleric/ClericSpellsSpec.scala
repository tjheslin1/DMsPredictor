package unit.cleric

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SacredFlame
import util.TestData._

class ClericSpellsSpec extends UnitSpecBase {

  "Sacred Flame" should {
    "deal 1d8 damage for a first level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelOne)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 8
    }

    "deal 1d8 damage for a second level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelTwo)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 8
    }

    "deal 2d8 damage for a fifth level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelFive)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 16
    }

    "deal 3d8 damage for a eleventh level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelEleven)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 24
    }

    "deal 4d8 damage for a seventeenth level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelSeventeen)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 32
    }

    "deal 4d8 damage for a twenty level spellCaster" in new TestContext {
      implicit val rollStrategy: RollStrategy = _ => RollResult(8)

      val cleric = random[Cleric]
        .withSpellKnown(SacredFlame)
        .withLevel(LevelTwenty)
        .asInstanceOf[Cleric]

      SacredFlame.damage(cleric, SacredFlame.spellLevel) shouldBe 32
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
