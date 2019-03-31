package unit

import base.{Tracking, UnitSpecBase}
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities.nextAbilityToUseInConjunction
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.extraAttack
import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.AbilityAction
import org.scalatest.OptionValues
import util.TestData._

class ClassAbilitiesSpec extends UnitSpecBase with OptionValues {

  "nextAbilityToUseInConjunction" should {
    "find the next ability lower in order" in {
      forAll { fighter: Fighter =>
        new TestContext {
          val combatant = fighter
            .withAbilities(List(trackedAbility(1), extraAttack(2), trackedAbility(3)))
            .withCombatIndex(1)

          val expected = trackedAbility(3)(combatant)
          val actual = nextAbilityToUseInConjunction(combatant,
                                                     List.empty[Combatant],
                                                     2,
                                                     AbilityAction.Any).value.apply(combatant)

          actual.name shouldBe expected.name
          actual.order shouldBe expected.order
        }
      }
    }
  }

  private class TestContext extends Tracking {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
