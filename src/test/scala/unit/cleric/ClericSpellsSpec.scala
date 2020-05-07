package unit.cleric

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells._
import org.apache.http.impl.entity.DisallowIdentityContentLengthStrategy
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

  "Guiding Bolt" should {

    "apply the GuidingBoltCondition to a target on a Critical Hit" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val rogue = random[Rogue].withCombatIndex(1)

      val Combatant(_, updatedRogue) = GuidingBolt.additionalEffect(rogue, CriticalHit)

      updatedRogue.conditions should contain theSameElementsAs List(GuidingBoltCondition())
    }

    "apply the GuidingBoltCondition to a target on a Hit" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val rogue = random[Rogue].withCombatIndex(1)

      val Combatant(_, updatedRogue) = GuidingBolt.additionalEffect(rogue, Hit)

      updatedRogue.conditions should contain theSameElementsAs List(GuidingBoltCondition())
    }

    "not apply the GuidingBoltCondition to a target on a Miss" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val rogue = random[Rogue].withCombatIndex(1)

      val Combatant(_, updatedRogue) = GuidingBolt.additionalEffect(rogue, Miss)

      updatedRogue.conditions shouldBe List.empty[Condition]
    }

    "not apply the GuidingBoltCondition to a target if not Critical Miss" in new TestContext {
      implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

      val rogue = random[Rogue].withCombatIndex(1)

      val Combatant(_, updatedRogue) = GuidingBolt.additionalEffect(rogue, CriticalMiss)

      updatedRogue.conditions shouldBe List.empty[Condition]
    }
  }

  "Guiding Bolt Condition" should {

    "set the creatures DefenseStatus to Disadvantage when applied" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

          val updatedRogue = GuidingBoltCondition().onConditionApplied(rogue)

          updatedRogue.defenseStatus shouldBe Disadvantage
        }
      }
    }

    "set the creatures DefenseStatus to Regular when the condition is handled" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

          val guidingBoltedRogue = rogue.withCondition(GuidingBoltCondition())

          val updatedRogue = GuidingBoltCondition().handleOnDamage(guidingBoltedRogue, 20)

          updatedRogue.defenseStatus shouldBe Regular
        }
      }
    }

    "set the creatures DefenseStatus to Regular when the condition runs out" in {
      forAll { rogue: Rogue =>
        new TestContext {
          override implicit val rollStrategy: RollStrategy = Dice.defaultRandomiser

          val disadvantageRogue = rogue.withDefenseStatus(Disadvantage)

          val updatedRogue = GuidingBoltCondition().onConditionRemoved(disadvantageRogue)

          updatedRogue.defenseStatus shouldBe Regular
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val rollStrategy: RollStrategy
  }
}
