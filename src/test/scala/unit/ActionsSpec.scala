package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.equipment.armour.Shield
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Weapon.fixedDamageWeapon
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Turned
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import util.TestData._
import util.TestMonster

class ActionsSpec extends UnitSpecBase {

  "rollAttack" should {

    "roll with Advantage if the attacking Creature has attackStatus set to Advantage" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val iterator = Iterator(RollResult(2), RollResult(20))

        val advantageFighter = fighter.withAttackStatus(Advantage).withCombatIndex(1)
        val monster          = testMonster.withArmourClass(30).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => iterator.next()) shouldBe 20
      }
    }

    "roll with Disadvantage if the attacking Creature has attackStatus set to Disadvantage" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val iterator = Iterator(RollResult(10), RollResult(1))

        val advantageFighter = fighter.withAttackStatus(Disadvantage).withCombatIndex(1)
        val monster          = testMonster.withArmourClass(1).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => iterator.next()) shouldBe 1
      }
    }

    "roll with Advantage if the target Creature has defenseStatus set to Disadvantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(RollResult(2), RollResult(20))

        val monster = goblin.withDefenseStatus(Disadvantage).withCombatIndex(2)

        rollAttack(fighter.withCombatIndex(1), monster)(_ => iterator.next()) shouldBe 20
      }
    }

    "roll with Disadvantage if the Creature has defenseStatus set to Advantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(RollResult(19), RollResult(1))

        val monster = goblin.withDefenseStatus(Advantage).withCombatIndex(2)

        rollAttack(fighter.withCombatIndex(1), monster)(_ => iterator.next()) shouldBe 1
      }
    }

    "roll regularly if attackStatus is Advantage and defenseStatus is Advantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(RollResult(19), RollResult(20))

        val advantageFighter = fighter.withAttackStatus(Advantage).withCombatIndex(1)
        val monster          = goblin.withArmourClass(1).withDefenseStatus(Advantage).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => iterator.next()) shouldBe 19
      }
    }

    "roll regularly if attackStatus is Disadvantage and defenseStatus is Disadvantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(RollResult(19), RollResult(20))

        val disadvantageFighter = fighter.withAttackStatus(Disadvantage).withCombatIndex(1)
        val monster             = goblin.withArmourClass(1).withDefenseStatus(Disadvantage).withCombatIndex(2)

        rollAttack(disadvantageFighter, monster)(_ => iterator.next()) shouldBe 19
      }
    }
  }

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = D20.naturalTwenty

          attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2)) shouldBe CriticalHit
        }
      }
    }

    "use Strength, hitBonus and proficiencyBonus to determine an attack result for a player" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val plusTwoWeapon = weaponWithHitBonus(2)

          val fighterCombatant = fighter
            .withProficiencyBonus(4) // + 4
            .withStrength(14) // + 2
            .withBaseWeapon(plusTwoWeapon) // + 2
            .withCombatIndex(1)

          attack(fighterCombatant,
                 plusTwoWeapon,
                 testMonster.withArmourClass(19).withCombatIndex(2)) shouldBe Miss
          attack(fighterCombatant,
                 plusTwoWeapon,
                 testMonster.withArmourClass(18).withCombatIndex(2)) shouldBe Hit
        }
      }
    }

    "use only hitBonus to determine an attack result for a monster" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(10)

          val plusTwoWeapon = weaponWithHitBonus(2)

          val monster = testMonster
            .withStrength(20)
            .withBaseWeapon(plusTwoWeapon)
            .withCombatIndex(1)

          val ac12Cleric = cleric.withOffHand(Shield).withDexterity(10).withNoArmour()
          val ac13Cleric = cleric.withOffHand(Shield).withDexterity(12).withNoArmour()

          attack(monster, plusTwoWeapon, ac13Cleric.withCombatIndex(2)) shouldBe Miss
          attack(monster, plusTwoWeapon, ac12Cleric.withCombatIndex(2)) shouldBe Hit
        }
      }
    }

    "hit a monster if the attack overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val ac10Monster = monster.withArmourClass(10)

          attack(fighter.withCombatIndex(1), fighter.weapon, ac10Monster.withCombatIndex(2)) shouldBe Hit
        }
      }
    }

    "miss a monster if the attack doesn't overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(2)
          val ac20Monster                          = monster.withArmourClass(30)

          attack(fighter.withCombatIndex(1), fighter.weapon, ac20Monster.withCombatIndex(2)) shouldBe Miss
        }
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(1)

          attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2)) shouldBe CriticalMiss
        }
      }
    }

    "score a CriticalHit against a target using a specific DetermineCritical strategy" in {
      forAll { (champion: Champion, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val levelThreeChampion = champion.withLevel(LevelThree)

          attack(levelThreeChampion.withCombatIndex(1),
                 levelThreeChampion.weapon,
                 monster.withCombatIndex(2)) shouldBe CriticalHit
        }
      }
    }
  }

  "resolveDamage" should {
    "handle conditions which trigger on damage" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val turnedFighter = fighter.withCondition(Turned(10, 10)).withCombatIndex(2)

          val (_, Combatant(_, updatedFighter: Fighter)) =
            resolveDamage(monster.withCombatIndex(1), turnedFighter, monster.baseWeapon, Hit)

          updatedFighter.conditions shouldBe List()
        }
      }
    }
  }

  "resolveDamageMainHand" should {
    "kill a monster if the damage is more than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val oneHundredDamageWeapon =
            fixedDamageWeapon("one hundred damage weapon",
                              Melee,
                              Slashing,
                              twoHands = true,
                              dmg = 100)

          val player = fighter.withStrength(10).withBaseWeapon(oneHundredDamageWeapon)

          val playerCombatant  = player.withCombatIndex(1)
          val monsterCombatant = monster.withHealth(50).withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, Hit) shouldBe (playerCombatant, monsterCombatant
            .withCreature(monster.withHealth(0)))
        }
      }
    }

    "fail to kill a monster if the damage is less than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val oneDamageWeapon =
            fixedDamageWeapon("one damage weapon", Melee, Slashing, twoHands = true, dmg = 1)

          val playerCombatant =
            fighter.withStrength(10).withBaseWeapon(oneDamageWeapon).withCombatIndex(1)
          val monsterCombatant = monster.withHealth(10).withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, CriticalHit)(D20.naturalTwenty) shouldBe
            (playerCombatant, monsterCombatant.withCreature(monster.withHealth(8)))
        }
      }
    }

    "deal at least one damage to a creature resistance to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          override implicit val roll: RollStrategy = _ => RollResult(19)

          val tenDamageWeapon =
            fixedDamageWeapon("ten damage weapon", Melee, Slashing, twoHands = true, dmg = 1)

          val playerCombatant =
            fighter.withStrength(10).withBaseWeapon(tenDamageWeapon).withCombatIndex(1)
          val modifiedMonster = monster.withResistance(Slashing).withHealth(100)

          val monsterCombatant = modifiedMonster
            .withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, Hit)(_ => 19) shouldBe
            (playerCombatant, monsterCombatant.withCreature(modifiedMonster.withHealth(99)))
        }
      }
    }
  }

  "runCombatantTimes" should {
    "executed provided function n times against the two combatants" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        var count = 0

        val f: (Combatant, Combatant) => (Combatant, Combatant) = (c1, c2) => {
          count += 1
          (c1, c2)
        }

        runCombatantTimes(5, fighter.withCombatIndex(1), monster.withCombatIndex(1), f)

        count shouldBe 5
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    def weaponWithHitBonus(bonus: Int) = Weapon("", Melee, Slashing, true, 1, wpnHitBonus = bonus)
  }
}
