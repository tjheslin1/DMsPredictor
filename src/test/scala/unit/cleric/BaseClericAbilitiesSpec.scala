package unit.cleric

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseClericAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Turned
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._
import util.TestMonster

class BaseClericAbilitiesSpec extends UnitSpecBase {

  val Priority = 1

  "turnUndead" should {
    "be triggered for Undead creatures" in {
      val cleric      = random[Cleric].withCombatIndex(1)
      val goblin      = random[Goblin].withCombatIndex(2)
      val zombieOne   = random[Zombie].withCombatIndex(3)
      val zombieTwo   = random[Zombie].withCombatIndex(3)
      val zombieThree = random[Zombie].withCombatIndex(3)

      val enemies = List(cleric, goblin, zombieOne, zombieTwo, zombieThree)

      turnUndead(Priority)(cleric).triggerMet(enemies) shouldBe true
    }

    "not be triggered for non Undead creatures" in {
      val cleric = random[Cleric].withCombatIndex(1)
      val goblin = random[Goblin].withCombatIndex(2)

      val enemies = List(cleric, goblin)

      turnUndead(Priority)(cleric).triggerMet(enemies) shouldBe false
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

    "apply the Turned condition on the Undead targets" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(15)

          val clericCombatant = cleric.withWisdom(24).withProficiencyBonus(2).withCombatIndex(1)

          val enemies = List(zombieOne.withCombatIndex(1),
                             zombieTwo.withCombatIndex(2),
                             goblin.withCombatIndex(2))

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie))) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List(Turned(17, 10))
          updatedZombieTwo.conditions shouldBe List(Turned(17, 10))
        }
      }
    }
  }

  "destroyUndead" should {
    "destroy (set health to 0) undead enemies who fail their save and are CR 1/2 or lower" in {
      forAll {
        (cleric: Cleric,
         zombieOne: Zombie,
         zombieTwo: Zombie,
         zombieThree: Zombie,
         goblin: Goblin) =>
          new TestContext {
            implicit override val roll: RollStrategy = _ => RollResult(10)

            val clericCombatant = cleric.withWisdom(10).withProficiencyBonus(2).withCombatIndex(1)

            val toughUndead = zombieOne.withWisdom(20).withCombatIndex(2)
            val weakUndead = zombieTwo.withWisdom(1).withCombatIndex(3)
            val weakHighCrUndead = zombieThree.withWisdom(1).withCombatIndex(4)

            val enemies = List(toughUndead, weakUndead, weakHighCrUndead, goblin.withCombatIndex(2))

            val (_,
                 List(Combatant(_, updatedUndeadOne: TestMonster),
                      Combatant(_, updatedUndeadTwo: TestMonster),
                      Combatant(_, updatedUndeadThree: TestMonster))) =
              destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

            updatedUndeadOne.health shouldBe zombieOne.health
            updatedUndeadTwo.health shouldBe 0

            updatedUndeadThree.health shouldBe zombieThree.health
            updatedUndeadThree.conditions should contain theSameElementsAs List(Turned(17, 10))
          }
      }
    }
  }

  private class TestContext {
    implicit val roll: RollStrategy = Dice.defaultRandomiser
  }
}
