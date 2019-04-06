package unit.rogue

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogueAbilities._
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

class BaseRogueAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "hide" should {
    "use the rogues bonus action" in {
      new TestContext {
        implicit override val roll: RollStrategy = _ => RollResult(19)

        val rogue = random[Rogue].withCombatIndex(1)

        val updatedRogue = hide(Priority)(rogue).update.asInstanceOf[Rogue]

        updatedRogue.bonusActionUsed shouldBe true
      }
    }

    "apply the unseen condition to the rogue" in {
      forAll { (rogue: Rogue, goblinOne: Goblin, goblinTwo: Goblin, zombie: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(15)

          val dexterousRogue = rogue.withProficiencyBonus(2).withDexterity(12).withCombatIndex(1)
          val goblinCombatant = goblinOne.withWisdom(10).withCombatIndex(2)
          val unwiseGoblin = goblinTwo.withWisdom(2).withCombatIndex(3)
          val wiseZombie = zombie.withWisdom(20).withCombatIndex(4)

          val (Combatant(_, updatedRogue: Rogue), _) =
            hide(Priority)(dexterousRogue).useAbility(List(goblinCombatant, unwiseGoblin, wiseZombie), LowestFirst)

          updatedRogue.hiddenFrom shouldBe List(goblinCombatant, unwiseGoblin)
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
