package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Weapon.fixedDamageWeapon
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._
import util.TestMonster

class ActionsSpec extends UnitSpecBase {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2))(_ => 20) shouldBe CriticalHit
      }
    }

    "hit a monster if the attack overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac10Monster = monster.copy(armourClass = 10)

        attack(fighter.withCombatIndex(1), fighter.weapon, ac10Monster.withCombatIndex(2))(_ => 19) shouldBe Hit
      }
    }

    "miss a monster if the attack doesn't overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac20Monster = monster.copy(armourClass = 30)

        attack(fighter.withCombatIndex(1), fighter.weapon, ac20Monster.withCombatIndex(2))(_ => 2) shouldBe Miss
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2))(_ => 1) shouldBe CriticalMiss
      }
    }

    "score a CriticalHit against a target using a specific DetermineCritical strategy" in {
      forAll { (champion: Champion, monster: TestMonster) =>
        val levelThreeChampion = champion.withLevel(LevelThree)

        attack(levelThreeChampion.withCombatIndex(1), levelThreeChampion.weapon, monster.withCombatIndex(2))(_ => 19) shouldBe CriticalHit
      }
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val oneHundredDamageWeapon = fixedDamageWeapon("one hundred damage weapon", Melee, Slashing, twoHands = true, dmg = 100)
        val player                 = fighter.withStrength(10).withBaseWeapon(oneHundredDamageWeapon)

        val playerCombatant  = player.withCombatIndex(1)
        val monsterCombatant = monster.withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit) shouldBe (playerCombatant, monsterCombatant.withCreature(
          monster.withHealth(0)))
      }
    }

    "fail to kill a monster if the damage is less than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val oneDamageWeapon = fixedDamageWeapon("one damage weapon", Melee, Slashing, twoHands = true,dmg =  1)

        val playerCombatant  = fighter.withStrength(10).withBaseWeapon(oneDamageWeapon).withCombatIndex(1)
        val monsterCombatant = monster.withHealth(10).withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, CriticalHit)(Dice.naturalTwenty) shouldBe
          (playerCombatant, monsterCombatant.withCreature(monster.withHealth(8)))
      }
    }

    "deal half damage rounded down to a creature resistance to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Melee, Slashing, twoHands = true, dmg = 11)

        val playerCombatant = fighter.withStrength(10).withBaseWeapon(tenDamageWeapon).withCombatIndex(1)
        val modifiedMonster = monster.withResistance(Slashing).withHealth(100)

        val monsterCombatant = modifiedMonster
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit)(_ => 19) shouldBe
          (playerCombatant, monsterCombatant.withCreature(modifiedMonster.withHealth(95)))
      }
    }

    "deal regular damage to a creature resistance to the damage type for a critical hit" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Melee, Slashing, twoHands = true, dmg = 11)

        val playerCombatant = fighter.withStrength(10).withBaseWeapon(tenDamageWeapon).withCombatIndex(1)
        val modifiedMonster = monster
          .withResistance(Slashing)
          .withHealth(100)

        val monsterCombatant = modifiedMonster
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, CriticalHit)(Dice.naturalTwenty) shouldBe
          (playerCombatant, monsterCombatant.withCreature(modifiedMonster.withHealth(89)))
      }
    }

    "deal no damage to a creature immune to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Melee, Slashing, twoHands = true, dmg = 10)

        val playerCombatant = fighter.withStrength(10).withBaseWeapon(tenDamageWeapon).withCombatIndex(1)
        val modifiedMonster = monster
          .withImmunity(Slashing)
          .withHealth(100)

        val monsterCombatant = modifiedMonster
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit)(_ => 19) shouldBe
          (playerCombatant, monsterCombatant.withCreature(modifiedMonster.withHealth(100)))
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
}
