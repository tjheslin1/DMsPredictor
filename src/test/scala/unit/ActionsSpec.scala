package unit

import base.{Tracking, UnitSpecBase}
import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.classes.ranger.Ranger
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.equipment.armour.Shield
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Weapon.fixedDamageWeapon
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, Turned}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SpiritGuardiansCondition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.RangerSpells._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Zombie}
import util.TestData._
import util.TestMonster

class ActionsSpec extends UnitSpecBase {

  "rollAttack" should {

    "roll with Advantage if the attacking Creature has attackStatus set to Advantage" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val iterator = Iterator(2, 20)

        val advantageFighter = fighter.withAttackStatus(Advantage).withCombatIndex(1)
        val monster          = testMonster.withArmourClass(30).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => RollResult(iterator.next())) shouldBe 20
      }
    }

    "roll with Disadvantage if the attacking Creature has attackStatus set to Disadvantage" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        val iterator = Iterator(10, 1)

        val advantageFighter = fighter.withAttackStatus(Disadvantage).withCombatIndex(1)
        val monster          = testMonster.withArmourClass(1).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => RollResult(iterator.next())) shouldBe 1
      }
    }

    "roll with Advantage if the target Creature has defenseStatus set to Disadvantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(2, 20)

        val monster = goblin.withDefenseStatus(Disadvantage).withCombatIndex(2)

        rollAttack(fighter.withCombatIndex(1), monster)(_ => RollResult(iterator.next())) shouldBe 20
      }
    }

    "roll with Disadvantage if the Creature has defenseStatus set to Advantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(19, 1)

        val monster = goblin.withDefenseStatus(Advantage).withCombatIndex(2)

        rollAttack(fighter.withCombatIndex(1), monster)(_ => RollResult(iterator.next())) shouldBe 1
      }
    }

    "roll regularly if attackStatus is Advantage and defenseStatus is Advantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(19, 20)

        val advantageFighter = fighter.withAttackStatus(Advantage).withCombatIndex(1)
        val monster          = goblin.withArmourClass(1).withDefenseStatus(Advantage).withCombatIndex(2)

        rollAttack(advantageFighter, monster)(_ => RollResult(iterator.next())) shouldBe 19
      }
    }

    "roll regularly if attackStatus is Disadvantage and defenseStatus is Disadvantage" in {
      forAll { (fighter: Fighter, goblin: Goblin) =>
        val iterator = Iterator(19, 20)

        val disadvantageFighter = fighter.withAttackStatus(Disadvantage).withCombatIndex(1)
        val monster             = goblin.withArmourClass(1).withDefenseStatus(Disadvantage).withCombatIndex(2)

        rollAttack(disadvantageFighter, monster)(_ => RollResult(iterator.next())) shouldBe 19
      }
    }
  }

  "attack" should {
    "hit if the attack roll was a natural 20" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = D20.naturalTwenty

          val (attackResult, _) =
            attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2))

          attackResult shouldBe CriticalHit
        }
      }
    }

    "use Strength, hitBonus and proficiencyBonus to determine an attack result for a player" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val plusTwoWeapon = weaponWithHitBonus(2)

          val fighterCombatant = fighter
            .withProficiencyBonus(4) // + 4
            .withStrength(14) // + 2
            .withDexterity(10)
            .withBaseWeapon(plusTwoWeapon) // + 2
            .withCombatIndex(1)

          val (attackResult, _) = attack(fighterCombatant,
                                         plusTwoWeapon,
                                         testMonster.withArmourClass(19).withCombatIndex(2))

          attackResult shouldBe Miss

          val (attackResult2, _) = attack(fighterCombatant,
                                          plusTwoWeapon,
                                          testMonster.withArmourClass(18).withCombatIndex(2))

          attackResult2 shouldBe Hit
        }
      }
    }

    "use Dexterity, hitBonus and proficiencyBonus to determine an attack result for a player with a finesse weapon" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val plusTwoFinesseWeapon =
            Weapon("test-weapon",
                   Melee,
                   Slashing,
                   isTwoHanded = true,
                   isFinesse = true,
                   1,
                   wpnHitBonus = 2)

          val fighterCombatant = fighter
            .withProficiencyBonus(4) // + 4
            .withDexterity(14) // + 2
            .withStrength(10)
            .withBaseWeapon(plusTwoFinesseWeapon) // + 2
            .withCombatIndex(1)

          val (attackResult, _) = attack(fighterCombatant,
                                         plusTwoFinesseWeapon,
                                         testMonster.withArmourClass(19).withCombatIndex(2))

          attackResult shouldBe Miss

          val (attackResult2, _) = attack(fighterCombatant,
                                          plusTwoFinesseWeapon,
                                          testMonster.withArmourClass(18).withCombatIndex(2))

          attackResult2 shouldBe Hit
        }
      }
    }

    "use only hitBonus to determine an attack result for a monster" in {
      forAll { (cleric: Cleric, testMonster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val plusTwoWeapon = weaponWithHitBonus(2)

          val monster = testMonster
            .withStrength(20)
            .withBaseWeapon(plusTwoWeapon)
            .withCombatIndex(1)

          val ac12Cleric = cleric.withOffHand(Shield).withDexterity(10).withNoArmour()
          val ac13Cleric = cleric.withOffHand(Shield).withDexterity(12).withNoArmour()

          val (attackResult, _) = attack(monster, plusTwoWeapon, ac13Cleric.withCombatIndex(2))
          attackResult shouldBe Miss

          val (attackResult2, _) = attack(monster, plusTwoWeapon, ac12Cleric.withCombatIndex(2))
          attackResult2 shouldBe Hit
        }
      }
    }

    "hit a monster if the attack overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val ac10Monster = monster.withArmourClass(10)

          val (attackResult, _) =
            attack(fighter.withCombatIndex(1), fighter.weapon, ac10Monster.withCombatIndex(2))

          attackResult shouldBe Hit
        }
      }
    }

    "miss a monster if the attack doesn't overcomes the monster's armour class" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(2)
          val ac20Monster                          = monster.withArmourClass(30)

          val (attackResult, _) =
            attack(fighter.withCombatIndex(1), fighter.weapon, ac20Monster.withCombatIndex(2))

          attackResult shouldBe Miss
        }
      }
    }

    "miss if the attack roll was a natural 1" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(1)

          val (attackResult, _) =
            attack(fighter.withCombatIndex(1), fighter.weapon, monster.withCombatIndex(2))

          attackResult shouldBe CriticalMiss
        }
      }
    }

    "score a CriticalHit against a target using a specific DetermineCritical strategy" in {
      forAll { (champion: Champion, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val levelThreeChampion = champion.withLevel(LevelThree)

          val (attackResult, _) = attack(levelThreeChampion.withCombatIndex(1),
                                         levelThreeChampion.weapon,
                                         monster.withCombatIndex(2))

          attackResult shouldBe CriticalHit
        }
      }
    }

    "handle available reaction on hit" in {
      val bonusHitWeapon = Weapon("bonus-hit",
                                  Melee,
                                  Slashing,
                                  isTwoHanded = false,
                                  isFinesse = false,
                                  4,
                                  wpnHitBonus = 5)

      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val attackingGoblin = goblin.withBaseWeapon(bonusHitWeapon).withCombatIndex(1)
          val wizardCombatant =
            wizard
              .withCastShieldOnReaction(true)
              .withDexterity(10)
              .withHealth(50)
              .withCombatIndex(2)

          val (_, Combatant(_, updatedWizard: Wizard)) =
            attack(attackingGoblin, attackingGoblin.creature.weapon, wizardCombatant)

          updatedWizard.reactionUsed shouldBe true
          updatedWizard.health shouldBe 50
        }
      }
    }

    "not use reaction if attack missed" in {
      forAll { (wizard: Wizard, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val attackingGoblin = goblin.withStrength(2).withCombatIndex(1)
          val highAcWizard = wizard
            .withCastShieldOnReaction(true)
            .withDexterity(28)
            .withHealth(50)
            .withCombatIndex(2)

          val (_, Combatant(_, updatedWizard: Wizard)) =
            attack(attackingGoblin, attackingGoblin.creature.weapon, highAcWizard)

          updatedWizard.reactionUsed shouldBe false
        }
      }
    }
  }

  "resolveDamage" should {
    "add a players strength modifier" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext with Tracking {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val strongFighter = fighter.withStrength(14).withDexterity(10).withCombatIndex(1)
          val monster =
            testMonster.withArmourClass(2).withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val (_, Combatant(_, updatedMonster: TestMonster), _) =
            resolveDamage(strongFighter, monster, List(), trackedSword, Hit)

          updatedMonster.health shouldBe 47
        }
      }
    }

    "add a players dexterity modifier if higher than strength for a finesse weapon" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext with Tracking {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val finesseWeapon =
            Weapon("sword", Melee, Slashing, isTwoHanded = false, isFinesse = true, dmg = 1)

          val strongFighter = fighter.withStrength(14).withDexterity(16).withCombatIndex(1)
          val monster =
            testMonster.withArmourClass(2).withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val (_, Combatant(_, updatedMonster: TestMonster), _) =
            resolveDamage(strongFighter, monster, List(), finesseWeapon, Hit)

          updatedMonster.health shouldBe 46
        }
      }
    }

    "add a players dexterity modifier for a Ranged weapon" in {
      forAll { (ranger: Ranger, testMonster: TestMonster) =>
        implicit val roll: RollStrategy = _ => RollResult(19)

        val rangedWeapon =
          Weapon("bow", Ranged, Piercing, isTwoHanded = true, isFinesse = false, dmg = 1)

        val dextrousRanger =
          ranger.withBaseWeapon(rangedWeapon).withDexterity(14).withStrength(2).withCombatIndex(1)

        val monster =
          testMonster.withArmourClass(2).withHealth(50).withMaxHealth(50).withCombatIndex(2)

        val (_, Combatant(_, updatedMonster: TestMonster), _) =
          resolveDamage(dextrousRanger, monster, List(), rangedWeapon, Hit)

        updatedMonster.health shouldBe 47
      }
    }

    "not add players modifier if addStatModifier is false" in {
      forAll { (fighter: Fighter, testMonster: TestMonster) =>
        new TestContext with Tracking {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val strongFighter = fighter.withStrength(16).withDexterity(10).withCombatIndex(1)

          val monster =
            testMonster.withArmourClass(2).withHealth(50).withMaxHealth(50).withCombatIndex(2)

          val (_, Combatant(_, updatedMonster: TestMonster), _) =
            resolveDamage(strongFighter,
                          monster,
                          List(),
                          trackedSword,
                          Hit,
                          damageBonus = 0,
                          addStatModifier = false)

          updatedMonster.health shouldBe 49
        }
      }
    }

    "handle conditions which trigger on damage" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val turnedFighter =
            fighter.withDexterity(5).withNoArmour().withCondition(Turned(10, 10)).withCombatIndex(2)

          val damagingMonster = monster.withStrength(18).withDexterity(18).withCombatIndex(1)

          val (_, Combatant(_, updatedFighter: Fighter), _) =
            resolveDamage(damagingMonster,
                          turnedFighter,
                          List.empty[Combatant],
                          monster.baseWeapon,
                          Hit)

          updatedFighter.conditions shouldBe List.empty[Condition]
        }
      }
    }

    "return all other combatants" in {
      new TestContext {
        implicit override val roll: RollStrategy = _ => RollResult(19)

        val fighter = random[Fighter].withCombatIndex(1)
        val cleric  = random[Cleric].withCombatIndex(2)
        val goblin  = random[Goblin].withCombatIndex(3)
        val zombie  = random[Zombie].withCombatIndex(4)

        val (Combatant(_, updatedFighter: Fighter),
             Combatant(_, updatedGoblin: Goblin),
             List(Combatant(_, updatedCleric: Cleric), Combatant(_, updatedZombie: Zombie))) =
          resolveDamage(fighter, goblin, List(cleric, zombie), fighter.creature.weapon, Miss)
      }
    }

    "handle loss of concentration of a ConcentrationConditionSpell" in {
      forAll { (cleric: Cleric, goblin: Goblin, zombie: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val concentratingCleric = cleric
            .withConcentratingOn(spiritGuardiansConcentrationSpell)
            .withHealth(50)
            .withMaxHealth(50)
            .withConstitution(2)
            .withCombatIndex(1)

          val spiritGuardiansCondition = SpiritGuardiansCondition(3, 10, 10, Wisdom)

          val goblinCombatant = goblin
            .withStrength(10)
            .withDexterity(10)
            .withCondition(spiritGuardiansCondition)
            .withCombatIndex(2)

          val zombieCombatant = zombie.withCondition(spiritGuardiansCondition).withCombatIndex(3)

          val (Combatant(_, updatedGoblin: Goblin),
               Combatant(_, updatedCleric: Cleric),
               List(Combatant(_, updatedZombie: Zombie))) =
            resolveDamage(goblinCombatant,
                          concentratingCleric,
                          List(zombieCombatant),
                          goblin.weapon,
                          Hit,
                          damageBonus = 30)

          updatedGoblin.conditions shouldBe List.empty[Condition]
          updatedZombie.conditions shouldBe List.empty[Condition]

          updatedCleric.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "handle loss of concentration of a SelfBuffSpell" in {
      forAll { (ranger: Ranger, zombie: Zombie) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val concentratingRanger = ranger
            .withConcentratingOn(HuntersMark)
            .withCondition(HuntersMarkBuffCondition)
            .withHealth(50)
            .withMaxHealth(50)
            .withConstitution(2)
            .withCombatIndex(1)

          val zombieCombatant = zombie.withCombatIndex(2)

          val (_, Combatant(_, updatedRanger: Ranger), _) =
            resolveDamage(zombieCombatant,
                          concentratingRanger,
                          List.empty[Combatant],
                          zombie.weapon,
                          Hit,
                          damageBonus = 30)

          updatedRanger.conditions shouldBe List.empty[Condition]
          updatedRanger.concentratingSpell shouldBe none[Spell]
        }
      }
    }

    "handle available reaction on damage" in {
      forAll { (rogue: Rogue, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val attackingGoblin =
            goblin.withStrength(10).withBaseWeapon(sixDamageWeapon).withCombatIndex(1)
          val levelFiveRogue = rogue.withLevel(LevelFive).withHealth(50).withCombatIndex(2)

          val (_, Combatant(_, updatedRogue: Rogue), _) =
            resolveDamage(attackingGoblin, levelFiveRogue, List(), sixDamageWeapon, Hit)

          updatedRogue.reactionUsed shouldBe true
          updatedRogue.health shouldBe 47
        }
      }
    }

    "not use reaction if no damage is taken" in {
      forAll { (rogue: Rogue, goblin: Goblin) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(10)

          val attackingGoblin = goblin.withStrength(10).withCombatIndex(1)
          val levelFiveRogue  = rogue.withLevel(LevelFive).withHealth(50).withCombatIndex(2)

          val (_, Combatant(_, updatedRogue: Rogue), _) =
            resolveDamage(attackingGoblin, levelFiveRogue, List(), goblin.weapon, Miss)

          updatedRogue.reactionUsed shouldBe false
          updatedRogue.health shouldBe 50
        }
      }
    }
  }

  "resolveDamageMainHand" should {
    "kill a monster if the damage is more than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val oneHundredDamageWeapon =
            fixedDamageWeapon("one hundred damage weapon",
                              Melee,
                              Slashing,
                              twoHands = true,
                              finesse = false,
                              dmg = 100)

          val player = fighter.withStrength(10).withBaseWeapon(oneHundredDamageWeapon)

          val playerCombatant  = player.withCombatIndex(1)
          val monsterCombatant = monster.withHealth(50).withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, List(), Hit) shouldBe (playerCombatant, monsterCombatant
            .withCreature(monster.withHealth(0)), List())
        }
      }
    }

    "fail to kill a monster if the damage is less than the monster's health" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val oneDamageWeapon =
            fixedDamageWeapon("one damage weapon",
                              Melee,
                              Slashing,
                              twoHands = true,
                              finesse = false,
                              dmg = 1)

          val playerCombatant =
            fighter.withStrength(10).withBaseWeapon(oneDamageWeapon).withCombatIndex(1)
          val monsterCombatant = monster.withHealth(10).withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, List(), CriticalHit)(
            D20.naturalTwenty) shouldBe
            (playerCombatant, monsterCombatant.withCreature(monster.withHealth(8)), List())
        }
      }
    }

    "deal at least one damage to a creature resistance to the damage type" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        new TestContext {
          implicit override val roll: RollStrategy = _ => RollResult(19)

          val tenDamageWeapon =
            fixedDamageWeapon("ten damage weapon",
                              Melee,
                              Slashing,
                              twoHands = true,
                              finesse = false,
                              dmg = 1)

          val playerCombatant =
            fighter.withStrength(10).withBaseWeapon(tenDamageWeapon).withCombatIndex(1)
          val modifiedMonster = monster.withResistance(Slashing).withHealth(100)

          val monsterCombatant = modifiedMonster
            .withCombatIndex(2)

          resolveDamageMainHand(playerCombatant, monsterCombatant, List(), Hit) shouldBe
            (playerCombatant, monsterCombatant.withCreature(modifiedMonster.withHealth(99)), List())
        }
      }
    }
  }

  "runCombatantTimes" should {
    "executed provided function n times against the two combatants" in {
      forAll { (fighter: Fighter, monster: TestMonster) =>
        var count = 0

        val f: (Combatant, Combatant, List[Combatant]) => (Combatant, Combatant, List[Combatant]) =
          (c1, c2, cs) => {
            count += 1
            (c1, c2, cs)
          }

        runCombatantTimes(5, fighter.withCombatIndex(1), monster.withCombatIndex(1), List(), f)

        count shouldBe 5
      }
    }
  }

  abstract private class TestContext {
    implicit val roll: RollStrategy

    val sixDamageWeapon =
      Weapon("six-damage-weapon", Melee, Slashing, isTwoHanded = true, isFinesse = false, 6)

    def weaponWithHitBonus(bonus: Int) =
      Weapon("", Melee, Slashing, isTwoHanded = true, isFinesse = false, 1, wpnHitBonus = bonus)

    val spiritGuardiansConcentrationSpell: Spell = new ConcentrationConditionSpell {
      val name: String = "test-concentration-spell"

      val attribute: Attribute  = Wisdom
      val singleTarget: Boolean = true

      val school: SchoolOfMagic    = Evocation
      val castingTime: CastingTime = OneActionCast
      val spellLevel: SpellLevel   = 1
      val useHigherSpellSlot       = true

      def conditionFrom(spellCaster: SpellCaster): Condition =
        SpiritGuardiansCondition(3, 10, 10, Wisdom)
    }
  }
}
