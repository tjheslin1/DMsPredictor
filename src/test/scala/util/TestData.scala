package util

import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  val DamageTypes = List(Bludgeoning, Piercing, Slashing)

  implicit class TestMonsterOps(val testMonster: TestMonster) extends AnyVal {
    import TestMonster._

    def withName(creatureName: String)                   = nameLens.set(creatureName)(testMonster)
    def withHealth(hp: Int)                              = healthLens.set(hp)(testMonster)
    def withMaxHealth(hp: Int)                           = maxHealthLens.set(hp)(testMonster)
    def withStrength(strengthScore: Stat)                = strengthLens.set(strengthScore)(testMonster)
    def withDexterity(dexScore: Stat)                    = dexterityLens.set(dexScore)(testMonster)
    def withConstitution(conScore: Stat)                 = constitutionLens.set(conScore)(testMonster)
    def withWeapon(weapon: Weapon)                       = baseWeaponLens.set(weapon)(testMonster)
    def withArmourClass(ac: Int)                         = armourClassLens.set(ac)(testMonster)
    def withResistance(creatureRes: DamageType*)         = resistancesLens.set(creatureRes.toList)(testMonster)
    def withImmunity(creatureImm: DamageType*)           = immunitiesLens.set(creatureImm.toList)(testMonster)
    def withNoResistances()                              = resistancesLens.set(List.empty)(testMonster)
    def withNoImmunities()                               = immunitiesLens.set(List.empty)(testMonster)
    def withNoResistancesOrImmunities()                  = testMonster.withNoResistances().withNoImmunities()

    def withAbilities(ablts: List[CreatureAbility]) = testMonster.copy(abilities = ablts)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    import Fighter._
    import FighterAbilities._

    def withLevel(lvl: Level)                            = levelLens.set(lvl)(fighter)
    def withName(creatureName: String)                   = nameLens.set(creatureName)(fighter)
    def withHealth(hp: Int)                              = healthLens.set(hp)(fighter)
    def withMaxHealth(hp: Int)                           = maxHealthLens.set(hp)(fighter)
    def withAllAbilitiesUsed()                           = fighterAbilityUsagesLens.set(allUsed())(fighter)
    def withAllAbilitiesUnused()                         = fighterAbilityUsagesLens.set(allUnused())(fighter)
    def withStrength(strengthScore: Stat)                = strengthLens.set(strengthScore)(fighter)
    def withDexterity(dexScore: Stat)                    = dexterityLens.set(dexScore)(fighter)
    def withConstitution(conScore: Stat)                 = constitutionLens.set(conScore)(fighter)
    def withWeapon(weapon: Weapon)                       = baseWeaponLens.set(weapon)(fighter)
    def withArmour(armr: Armour)                         = armourLens.set(armr)(fighter)
    def withNoArmour()                                   = armourLens.set(NoArmour)(fighter)
    def withShield()                                     = offHandLens.set(Shield().some)(fighter)
    def withOffHand(equipment: Equipment)                = offHandLens.set(equipment.some)(fighter)
    def withNoShield()                                   = offHandLens.set(None)(fighter)
    def withResistance(creatureRes: DamageType*)         = resistancesLens.set(creatureRes.toList)(fighter)
    def withImmunity(creatureImm: DamageType*)           = immunitiesLens.set(creatureImm.toList)(fighter)
    def withNoResistances()                              = resistancesLens.set(List.empty)(fighter)
    def withNoImmunities()                               = immunitiesLens.set(List.empty)(fighter)
    def withNoResistancesOrImmunities()                  = fighter.withNoResistances().withNoImmunities()
    def withFightingStyle(styles: FighterFightingStyle*) = fightingStylesLens.set(styles.toList)(fighter)
    def withNoFightingStyles()                           = fightingStylesLens.set(List.empty)(fighter)
  }

  implicit class PlayerOps[T <: Creature](val t: T) extends AnyVal {
    def withCombatIndex(index: Int) = Combatant(index, t)
  }

  implicit class CombatantOps(val combatant: Combatant) extends AnyVal {
    def withCreature(c: Creature) = combatant.copy(creature = c)
  }
}

trait TestData extends RandomDataGenerator {

  implicit val arbStat: Arbitrary[Stat] =
    Arbitrary {
      Gen
        .choose(1, 30)
        .map(refined.refineV[Interval.ClosedOpen[W.`1`.T, W.`31`.T]](_))
        .flatMap {
          case Right(i) => Gen.const(i)
          case Left(_)  => Gen.fail
        }
    }

  implicit val arbBaseStats: Arbitrary[BaseStats] = cachedImplicit

  implicit val arbDamageType: Arbitrary[DamageType] = Arbitrary {
    Gen.oneOf(Bludgeoning, Piercing, Slashing, Magical)
  }

  implicit val arbWeaponType: Arbitrary[WeaponType] = Arbitrary {
    Gen.oneOf(Melee, Ranged)
  }

  implicit val arbLevel: Arbitrary[Level] = Arbitrary {
    Gen.oneOf(LevelOne, LevelTwo, LevelThree, LevelFour, LevelFive)
  }

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName       <- Gen.alphaStr
      wpnType          <- arbWeaponType.arbitrary
      weaponDamageType <- arbDamageType.arbitrary
      twoHands         <- Gen.oneOf(true, false)
      wpnHitBonus      <- Gen.choose(0, 3)
      sides            <- Gen.choose(1, 12)
    } yield
      new Weapon {
        val name: String = weaponName
        val weaponType   = wpnType
        val damageType   = weaponDamageType
        val twoHanded    = twoHands

        override val hitBonus: Int = wpnHitBonus

        def damage(implicit rollStrategy: RollStrategy): Int = Dice.defaultRandomiser(sides)

      }
  }

  implicit val arbArmour: Arbitrary[Armour] = Arbitrary {
    for {
      armourName <- Gen.alphaStr
      baseArmour <- Gen.choose(5, 14)
    } yield
      new Armour {
        val name: String = armourName

        def armourClass(dexterity: Stat): Int = baseArmour + Modifier.mod(dexterity)
      }
  }

  implicit val arbShield: Arbitrary[Option[Shield]] = Arbitrary {
    Gen.oneOf(none[Shield], Shield().some) // TODO Gen any type of equipment
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      n         <- Gen.alphaStr
      hp        <- Gen.choose(10, 80)
      baseStats <- arbBaseStats.arbitrary
      wpn       <- arbWeapon.arbitrary
      armr    <- arbArmour.arbitrary
      optShield         <- arbShield.arbitrary
      cType     <- Gen.oneOf(PlayerCharacter, EnemyMonster)
      profBonus <- Gen.choose(0, 6)
    } yield
      new Creature {
        val creatureType: CreatureType = cType
        val health: Int                = hp
        val maxHealth: Int = hp
        val stats: BaseStats           = baseStats
        def weapon[_: RS]: Weapon      = wpn

        val armour: Armour = armr
        val offHand: Option[Equipment] = optShield

        val armourClass: Int           = armour.armourClass(stats.dexterity)

        val proficiencyBonus: Int = profBonus

        def updateHealth(modification: Int): Creature =
          throw new NotImplementedError("Impossible to implement, results in recursive definition of Creature")

        val abilities: List[CreatureAbility] = List.empty // TODO add core abilities
        val resistances: List[DamageType] = List.empty
        val immunities: List[DamageType]  = List.empty
        val name: String                  = n
      }
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      TestMonster(
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.weapon(Dice.defaultRandomiser),
        List.empty, // TODO add core abilities
        creature.resistances,
        creature.immunities,
        creature.name
      )
  }

  implicit val arbFighterFightingStyle: Arbitrary[Seq[FighterFightingStyle]] = Arbitrary {
    Gen.someOf(Archery, Defense, Dueling, GreatWeaponFighting, Protection, TwoWeaponFighting)
  }

  implicit val arbFighterAbilities: Arbitrary[FighterAbilities] = Arbitrary {
    for {
      secondWindUsed  <- Gen.oneOf(true, false)
      actionSurgeUsed <- Gen.oneOf(true, false)
    } yield FighterAbilities(secondWindUsed, actionSurgeUsed)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      abilities      <- arbFighterAbilities.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Fighter(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.weapon(Dice.defaultRandomiser),
        creature.armour,
        creature.offHand,
        fightingStyles.toList,
        abilities,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        creature.name
      )
  }

  implicit val arbChampion: Arbitrary[Champion] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      abilities      <- arbFighterAbilities.arbitrary
      armour         <- arbArmour.arbitrary
      shield         <- arbShield.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Champion(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.weapon(Dice.defaultRandomiser),
        armour,
        shield,
        fightingStyles.toList,
        abilities,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        creature.name
      )
  }
}
