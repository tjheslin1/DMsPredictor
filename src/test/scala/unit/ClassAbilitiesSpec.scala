package unit

import base.UnitSpecBase
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes._
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import org.scalatest.OptionValues
import util.TestData._

class ClassAbilitiesSpec extends UnitSpecBase with OptionValues {

  "nextAbilityToUseInConjunction" should {
    "find the next ability of lower priority" in {
      forAll { fighter: Fighter =>
        new TestContext {
          val combatant = fighter
            .withAbilities(List(1 -> dummyAbility("ignored"), 2 -> extraAttack, 3 -> dummyAbility("expected")))
            .withCombatIndex(1)

          ClassAbilities.nextAbilityToUseInConjunction(combatant, 1 -> extraAttack).value
          shouldBe dummyAbility("expected")
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser

    def dummyAbility(abilityName: String)(combatant: Combatant): Ability = new Ability(combatant) {
      val name: String = abilityName

      val levelRequirement: Level = LevelOne
      val triggerMet: Boolean     = true
      val conditionMet: Boolean   = true

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = (combatant, target)

      def update: Creature = combatant.creature}
    }
  }
}
