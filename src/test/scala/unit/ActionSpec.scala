package unit

import base.UnitSpecBase
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Weapon.fixedDamageWeapon
import io.github.tjheslin1.dmspredictor.model._
import util.TestData._

class ActionSpec extends UnitSpecBase {

  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        attack(fighter.creature.withCombatIndex(1), monster.creature.withCombatIndex(2))(_ => 20) shouldBe CriticalHit
      }
    }

    "hit a monster if the attack overcomes the monsters armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac10Monster = monster.creature.copy(armourClass = 10)

        attack(fighter.creature.withCombatIndex(1), ac10Monster.withCombatIndex(2))(_ => 19) shouldBe Hit
      }
    }

    "miss a monster if the attack overcomes the monsters armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val ac20Monster = monster.creature.copy(armourClass = 20)

        attack(fighter.creature.withCombatIndex(1), ac20Monster.withCombatIndex(2))(_ => 2) shouldBe Miss
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        attack(fighter.creature.withCombatIndex(1), monster.creature.withCombatIndex(2))(_ => 1) shouldBe CriticalMiss
      }
    }
  }

  "resolveDamage" should {
    "kill a monster if the damage is more than the monsters health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val oneHundredDamageWeapon = fixedDamageWeapon("one hundred damage weapon", Slashing, 100)
        val player                 = fighter.creature.withStrength(10).withWeapon(oneHundredDamageWeapon)

        val playerCombatant  = player.withCombatIndex(1)
        val monsterCombatant = monster.creature.withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit) shouldBe (playerCombatant, monsterCombatant
          .withHealth(0))
      }
    }

    "fail to kill a monster if the damage is less than the monsters health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val oneDamageWeapon = fixedDamageWeapon("one damage weapon", Slashing, 1)

        val playerCombatant  = fighter.creature.withStrength(10).withWeapon(oneDamageWeapon).withCombatIndex(1)
        val monsterCombatant = monster.creature.withHealth(10).withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, CriticalHit)(Dice.naturalTwenty) shouldBe
          (playerCombatant, monsterCombatant.withHealth(8))
      }
    }

    "deal half damage rounded down to a creature resistance to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Slashing, 11)

        val playerCombatant = fighter.creature.withStrength(10).withWeapon(tenDamageWeapon).withCombatIndex(1)
        val monsterCombatant = monster.creature
          .withResistance(Slashing)
          .withHealth(100)
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit)(_ => 19) shouldBe
          (playerCombatant, monsterCombatant.withHealth(95))
      }
    }

    "deal regular damage to a creature resistance to the damage type for a critical hit" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Slashing, 11)

        val playerCombatant = fighter.creature.withStrength(10).withWeapon(tenDamageWeapon).withCombatIndex(1)
        val monsterCombatant = monster.creature
          .withResistance(Slashing)
          .withHealth(100)
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, CriticalHit)(Dice.naturalTwenty) shouldBe
          (playerCombatant, monsterCombatant.withHealth(89))
      }
    }

    "deal no damage to a creature immune to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        val tenDamageWeapon = fixedDamageWeapon("ten damage weapon", Slashing, 10)

        val playerCombatant = fighter.creature.withStrength(10).withWeapon(tenDamageWeapon).withCombatIndex(1)
        val monsterCombatant = monster.creature
          .withImmunity(Slashing)
          .withHealth(100)
          .withCombatIndex(2)

        resolveDamage(playerCombatant, monsterCombatant, Hit)(_ => 19) shouldBe
          (playerCombatant, monsterCombatant.withHealth(100))
      }
    }
  }
}
