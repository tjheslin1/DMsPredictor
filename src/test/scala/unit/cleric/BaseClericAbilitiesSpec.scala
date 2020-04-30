package unit.cleric

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.BaseClericAbilities._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned, TurnedCondition}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import io.github.tjheslin1.dmspredictor.strategy.LowestFirst
import util.TestData._

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

    "not be used if Channel Divinity is already used" in {
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

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val enemies = List(zombieOne.withCombatIndex(2),
                             zombieTwo.withCombatIndex(3),
                             goblin.withCombatIndex(4))

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie),
                    _)) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List(Turned(17, 10))
          updatedZombieTwo.conditions shouldBe List(Turned(17, 10))
        }
      }
    }

    "apply the Turned condition to Undead targets if they fail their saving throws with advantage" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie, goblin: Goblin) =>
        new TestContext {
          val iterator = Iterator(
            5, // zombieOne saving throw roll
            1,
            2, // zombieTwo saving throw rolls with advantage
            1,
            20 // goblin saving throw rolls with advantage
          )

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val enemies = List(
            zombieOne.withCombatIndex(2),
            zombieTwo.withConditionResistance(TurnedCondition).withCombatIndex(3),
            goblin.withConditionResistance(TurnedCondition).withCombatIndex(4)
          )

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie),
                    Combatant(_, updatedGoblin: Goblin))) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List(Turned(17, 10))
          updatedZombieTwo.conditions shouldBe List(Turned(17, 10))
          updatedGoblin.conditions shouldBe List.empty[Condition]
        }
      }
    }

    "not apply the Turned condition to Undead targets if they have immunity to the Turned condition" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val enemies = List(
            zombieOne.withWisdom(2).withCombatIndex(2),
            zombieTwo.withWisdom(2).withConditionImmunity(TurnedCondition).withCombatIndex(3),
            goblin.withWisdom(2).withConditionImmunity(TurnedCondition).withCombatIndex(4)
          )

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie),
                    Combatant(_, updatedGoblin: Goblin))) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List(Turned(17, 10))
          updatedZombieTwo.conditions shouldBe List.empty[Condition]
          updatedGoblin.conditions shouldBe List.empty[Condition]
        }
      }
    }
  }

  // TODO check test failures

  "destroyUndead" should {
    "destroy undead enemies who fail their save and are CR 1/2 or lower" in {
      forAll {
        (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie, vampire: Vampire, goblin: Goblin) =>
          new TestContext {
            val iterator = Iterator(
              10, // zombieOne saving throw
              10, // zombieTwo saving throw
              6 // weak vampire saving throw
            )

            implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

            val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(18).withCombatIndex(1)

            val toughUndead = zombieOne.withWisdom(20).withCombatIndex(2)
            val weakUndead  = zombieTwo.withWisdom(1).withCombatIndex(3)
            val weakVampire =
              vampire.withNoLegendaryResistancesLeft().withWisdom(1).withCombatIndex(4)

            val enemies = List(toughUndead, weakUndead, weakVampire, goblin.withCombatIndex(5))

            val (_,
                 List(Combatant(_, updatedToughUndead: Zombie),
                      Combatant(_, updatedWeakUndead: Zombie),
                      Combatant(_, updatedVampire: Vampire),
                      _)) =
              destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

            updatedToughUndead.health shouldBe zombieOne.health
            updatedWeakUndead.health shouldBe 0
            updatedWeakUndead.isAlive shouldBe false

            updatedVampire.health shouldBe weakVampire.creature.health
            updatedVampire.conditions should contain theSameElementsAs List(Turned(14, 10))
          }
      }
    }

    "turn undead enemies who fail their save and are above CR 1/2" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, lich: Lich) =>
        new TestContext {
          val iterator = Iterator(
            10, // toughUndead saving throw
            1,
            1 // weak lich saving throw with advantage
          )

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(18).withCombatIndex(1)

          val toughZombie   = zombieOne.withWisdom(20).withCombatIndex(2)
          val lowWisdomLich = lich.withNoLegendaryResistancesLeft().withWisdom(1).withCombatIndex(4)

          val enemies = List(toughZombie, lowWisdomLich)

          val (_, List(Combatant(_, updatedToughZombie: Zombie), Combatant(_, updatedLich: Lich))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedToughZombie.health shouldBe zombieOne.health
          updatedToughZombie.isAlive shouldBe true

          updatedLich.health shouldBe lowWisdomLich.creature.health
          updatedLich.conditions should contain theSameElementsAs List(Turned(14, 10))
        }
      }
    }

    "apply the Turned condition to Undead targets if they fail their saving throws with advantage" in {
      forAll { (cleric: Cleric, vampireOne: Vampire, vampireTwo: Vampire, vampireThree: Vampire) =>
        new TestContext {
          val iterator = Iterator(
            5, // vampireOne saving throw roll
            1,
            2, // vampireTwo saving throw rolls with advantage
            1,
            20 // vampireThree saving throw rolls with advantage
          )

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val vampireCombatantOne = vampireOne.withNoLegendaryResistancesLeft().withCombatIndex(2)

          val vampireCombatantTwo = vampireTwo
            .withNoLegendaryResistancesLeft()
            .withConditionResistance(TurnedCondition)
            .withCombatIndex(3)

          val vampireCombatantThree = vampireThree
            .withNoLegendaryResistancesLeft()
            .withConditionResistance(TurnedCondition)
            .withCombatIndex(4)

          val enemies = List(vampireCombatantOne, vampireCombatantTwo, vampireCombatantThree)

          val (_,
               List(Combatant(_, updatedVampireOne: Vampire),
                    Combatant(_, updatedVampireTwo: Vampire),
                    Combatant(_, updatedVampireThree: Vampire))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedVampireOne.conditions shouldBe List(Turned(17, 10))
          updatedVampireTwo.conditions shouldBe List(Turned(17, 10))
          updatedVampireThree.conditions shouldBe List.empty[Condition]
        }
      }
    }

    "not apply the Turned condition to Undead targets if they have immunity to the Turned condition" in {
      forAll { (cleric: Cleric, vampireOne: Vampire, vampireTwo: Vampire, vampireThree: Vampire) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val vampireCombatantOne =
            vampireOne.withNoLegendaryResistancesLeft().withWisdom(2).withCombatIndex(2)

          val vampireCombatantTwo = vampireTwo
            .withNoLegendaryResistancesLeft()
            .withWisdom(2)
            .withConditionImmunity(TurnedCondition)
            .withCombatIndex(3)

          val vampireCombatantThree = vampireThree
            .withNoLegendaryResistancesLeft()
            .withWisdom(2)
            .withConditionImmunity(TurnedCondition)
            .withCombatIndex(4)

          val enemies = List(
            vampireCombatantOne,
            vampireCombatantTwo,
            vampireCombatantThree
          )

          val (_,
               List(Combatant(_, updatedVampireOne: Vampire),
                    Combatant(_, updatedVampireTwo: Vampire),
                    Combatant(_, updatedVampireThree: Vampire))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedVampireOne.conditions shouldBe List(Turned(17, 10))
          updatedVampireTwo.conditions shouldBe List.empty[Condition]
          updatedVampireThree.conditions shouldBe List.empty[Condition]
        }
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy
  }
}
