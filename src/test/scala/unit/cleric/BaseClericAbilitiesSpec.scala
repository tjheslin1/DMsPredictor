package unit.cleric

import base.UnitSpecBase
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseClericAbilities.turnUndead
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import util.TestData._

class BaseClericAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "turnUndead" should {
    "only target Undead creature" in {
      val cleric = random[Cleric].withCombatIndex(1)
      val goblin = random[Goblin].withCombatIndex(2)
      val zombie = random[Zombie].withCombatIndex(3)

      turnUndead(Priority)(cleric).triggerMet(goblin.some) shouldBe false
      turnUndead(Priority)(cleric).triggerMet(zombie.some) shouldBe true
    }

    "not be used if already used" in {
      val cleric = random[Cleric].withChannelDivinityUsed().withCombatIndex(1)

      turnUndead(Priority)(cleric).conditionMet shouldBe false
    }

    "update the BaseCleric's channelDivinityUsed to true" in {
      val cleric = random[Cleric].withCombatIndex(1)

      val updatedCleric = turnUndead(Priority)(cleric).update.asInstanceOf[BaseCleric]

      updatedCleric.channelDivinityUsed shouldBe true
    }

    "apply the Turned condition on the Undead target" in {
      forAll { (cleric: Cleric, zombie: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(15)

          val clericCombatant = cleric.withCombatIndex(1)
          val monster         = zombie.withCombatIndex(2)

          val (_, Some(Combatant(_, updatedZombie: Zombie))) =
            turnUndead(Priority)(clericCombatant).useAbility(monster.some)

          updatedZombie.conditions shouldBe List(Turned(15, 10))
        }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
