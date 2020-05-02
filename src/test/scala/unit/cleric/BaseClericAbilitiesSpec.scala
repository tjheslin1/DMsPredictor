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
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie) =>
        new TestContext {
          // format: off
          val iterator = Iterator(
            1, 20, // zombieOne saving throw roll
            1, 2, // zombieTwo saving throw rolls with advantage
          )
          // format: on

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val enemies = List(
            zombieOne.withWisdom(10).withConditionResistance(TurnedCondition).withCombatIndex(2),
            zombieTwo.withWisdom(10).withConditionResistance(TurnedCondition).withCombatIndex(3)
          )

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie))) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List.empty[Condition]
          updatedZombieTwo.conditions shouldBe List(Turned(17, 10))
        }
      }
    }

    "not apply the Turned condition to Undead targets if they have immunity to the Turned condition" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(5)

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val enemies = List(
            zombieOne.withWisdom(2).withCombatIndex(2),
            zombieTwo.withWisdom(2).withConditionImmunity(TurnedCondition).withCombatIndex(3),
          )

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie))) =
            turnUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List(Turned(17, 10))
          updatedZombieTwo.conditions shouldBe List.empty[Condition]
        }
      }
    }
  }

  "destroyUndead" should {
    "destroy undead enemies who fail their save and are CR 1/2 or lower" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie, goblin: Goblin) =>
        new TestContext {
          val iterator = Iterator(
            10, // zombieOne saving throw
            10, // zombieTwo saving throw
            6 // weak goblin saving throw
          )

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(18).withCombatIndex(1)

          val toughUndead = zombieOne.withWisdom(20).withCombatIndex(2)
          val weakUndead  = zombieTwo.withWisdom(1).withCombatIndex(3)
          val goblinCombatant  = goblin.withCombatIndex(4)

          val enemies = List(toughUndead, weakUndead, goblinCombatant)

          val (_,
               List(Combatant(_, updatedToughUndead: Zombie),
                    Combatant(_, updatedWeakUndead: Zombie),
                    Combatant(_, updatedGoblin: Goblin))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedToughUndead.health shouldBe zombieOne.health

          updatedWeakUndead.health shouldBe 0
          updatedWeakUndead.isAlive shouldBe false

          updatedGoblin.health shouldBe goblinCombatant.creature.health
          updatedGoblin.isAlive shouldBe true
        }
      }
    }

    "turn undead enemies who fail their save and are above CR 1/2" in {
      forAll { (cleric: Cleric, vampireOne: Vampire, vampireTwo: Vampire, lich: Lich) =>
        new TestContext {
          //format: off
          val iterator = Iterator(
            18,     // strong vampire saving throw
            1,      // weak vampire saving throw
            1, 1    // weak lich saving throw with advantage
          )
          // format: on

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(18).withCombatIndex(1)

          val toughVampire = vampireOne.withNoLegendaryResistancesLeft().withWisdom(20).withCombatIndex(2)
          val weakVampire  = vampireTwo.withNoLegendaryResistancesLeft().withWisdom(1).withCombatIndex(3)
          val weakLich     = lich.withNoLegendaryResistancesLeft().withWisdom(1).withCombatIndex(4)

          val enemies = List(toughVampire, weakVampire, weakLich)

          val (_,
               List(Combatant(_, updatedToughVampire: Vampire),
                    Combatant(_, updatedWeakVampire: Vampire),
                    Combatant(_, updatedLich: Lich))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedToughVampire.conditions should contain theSameElementsAs List.empty[Condition]

          updatedWeakVampire.conditions should contain theSameElementsAs List(Turned(14, 10))

          updatedLich.conditions should contain theSameElementsAs List(Turned(14, 10))
        }
      }
    }

    "destroy Undead targets of CR 1/2 or below if they fail their saving throws with advantage" in {
      forAll { (cleric: Cleric, zombieOne: Zombie, zombieTwo: Zombie) =>
        new TestContext {
          // format: off
          val iterator = Iterator(
            1, 20, // zombieOne saving throw rolls with advantage
            1, 2, // zombieTwo saving throw rolls with advantage
          )
          // format: on

          implicit override val roll: RollStrategy = _ => RollResult(iterator.next())

          val clericCombatant = cleric.withProficiencyBonus(2).withWisdom(24).withCombatIndex(1)

          val zombieCombatantOne = zombieOne.withConditionResistance(TurnedCondition).withCombatIndex(2)

          val zombieCombatantTwo = zombieTwo
            .withConditionResistance(TurnedCondition)
            .withCombatIndex(3)

          val enemies = List(zombieCombatantOne, zombieCombatantTwo)

          val (_,
               List(Combatant(_, updatedZombieOne: Zombie),
                    Combatant(_, updatedZombieTwo: Zombie))) =
            destroyUndead(Priority)(clericCombatant).useAbility(enemies, LowestFirst)

          updatedZombieOne.conditions shouldBe List.empty[Condition]
          updatedZombieOne.health shouldBe zombieOne.health
          updatedZombieOne.isAlive shouldBe true

          updatedZombieTwo.conditions shouldBe List.empty[Condition]
          updatedZombieTwo.health shouldBe 0
          updatedZombieTwo.isAlive shouldBe false
        }
      }
    }

    "turn undead enemies of above CR 1/2 if they fail their saving throws with advantage" in {
      forAll { (cleric: Cleric, vampireOne: Vampire, vampireTwo: Vampire, vampireThree: Vampire) =>
        new TestContext {
          // format: off
          val iterator = Iterator(
            5,      // vampireOne saving throw roll
            1, 2,   // vampireTwo saving throw rolls with advantage
            1, 20   // vampireThree saving throw rolls with advantage
          )
          // format: on

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
