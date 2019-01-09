package util

import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.fighter.{Fighter, _}
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model.ProficiencyBonus.ProficiencyBonus
import io.github.tjheslin1.dmspredictor.model._
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  val DamageTypes = List(Bludgeoning, Piercing, Slashing)

  implicit class CombatantOps(val combatant: Combatant) extends AnyVal {
    import Combatant._

    def withCreature(c: Creature) = creatureLens.set(c)(combatant)
  }

  implicit class TestMonsterOps(val testMonster: TestMonster) extends AnyVal {
    def withName(creatureName: String)           = TestMonster._name.set(creatureName)(testMonster)
    def withHealth(hp: Int)                      = TestMonster._health.set(hp)(testMonster)
    def withMaxHealth(hp: Int)                   = TestMonster._maxHealth.set(hp)(testMonster)
    def withStrength(strengthScore: Stat)        = TestMonster.strengthLens.set(strengthScore)(testMonster)
    def withBaseWeapon(weapon: Weapon)               = TestMonster._baseWeapon.set(weapon)(testMonster)
    def withArmourClass(ac: Int)                 = TestMonster._armourClass.set(ac)(testMonster)
    def withResistance(creatureRes: DamageType*) = TestMonster._resistances.set(creatureRes.toList)(testMonster)
    def withImmunity(creatureImm: DamageType*)   = TestMonster._immunities.set(creatureImm.toList)(testMonster)
    def withNoResistances()                      = TestMonster._resistances.set(List.empty)(testMonster)
    def withNoImmunities()                       = TestMonster._immunities.set(List.empty)(testMonster)
    def withNoResistancesOrImmunities()          = testMonster.withNoResistances().withNoImmunities()

    def withAbilities(ablts: List[CreatureAbility]) = TestMonster._abilities.set(ablts)(testMonster)
  }

  implicit class CreatureOps(val creature: Creature) extends AnyVal {
    import Creature._

    def withHealth(hp: Int)                         = creatureHealthLens.set(hp)(creature)
    def withMaxHealth(hp: Int)                      = creatureMaxHealthLens.set(hp)(creature)
    def withStats(baseStats: BaseStats)             = creatureStatsLens.set(baseStats)(creature)
    def withBaseWeapon(baseWeapon: Weapon)          = creatureBaseWeaponLens.set(baseWeapon)(creature)
    def withArmour(armour: Armour)                  = creatureArmourLens.set(armour)(creature)
    def withOffHand(offHand: Equipment)     = creatureOffHandLens.set(offHand.some)(creature)
    def withArmourClass(ac: Int)                    = creatureArmourClassOptional.set(ac)(creature)
    def withAbilities(ablts: List[CreatureAbility]) = creatureAbilitiesLens.set(ablts)(creature)

    def withProficiencyBonusLens(proficiencyBonus: ProficiencyBonus) =
      creatureProficiencyBonusOptional.set(proficiencyBonus)(creature)

    def withStrength(strengthScore: Stat) = creatureStrengthLens.set(strengthScore)(creature)
    def withDexterity(dexScore: Stat)     = creatureDexterityLens.set(dexScore)(creature)
    def withConstitution(conScore: Stat)  = creatureConstitutionLens.set(conScore)(creature)

    def withNoArmour()  = creatureArmourLens.set(NoArmour)(creature)
    def withNoOffHand() = creatureOffHandLens.set(none[Equipment])(creature)

    def withResistance(creatureRes: DamageType*) = creatureResistancesLens.set(creatureRes.toList)(creature)
    def withImmunity(creatureImm: DamageType*)   = creatureImmunitiesLens.set(creatureImm.toList)(creature)
    def withNoResistances()                      = creatureResistancesLens.set(List.empty)(creature)
    def withNoImmunities()                       = creatureImmunitiesLens.set(List.empty)(creature)
    def withNoResistancesOrImmunities()          = creature.withNoResistances().withNoImmunities()

    def withLevel(level: Level)     = creatureLevelOptional.set(level)(creature)
    def withCombatIndex(index: Int) = Combatant(index, creature)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    import Fighter._

    def withFightingStyle(fightingStyle: FighterFightingStyle) = _fightingStyles.set(List(fightingStyle))(fighter)
    def withAllAbilitiesUnused() = _abilityUsages.set(FighterAbilities(false, false))(fighter)
    def withAllAbilitiesUsed() = _abilityUsages.set(FighterAbilities(true, true))(fighter)
  }
}

trait TestData extends RandomDataGenerator {

  implicit val arbProficiencyBonus: Arbitrary[ProficiencyBonus] =
    Arbitrary {
      Gen
        .choose(0, 6)
        .map(refined.refineV[Interval.ClosedOpen[W.`0`.T, W.`7`.T]](_))
        .flatMap {
          case Right(i) => Gen.const(i)

          case Left(_)  => Gen.fail
        }
    }

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
      armr      <- arbArmour.arbitrary
      optShield <- arbShield.arbitrary
      cType     <- Gen.oneOf(PlayerCharacter, Monster)
      profBonus <- arbProficiencyBonus.arbitrary
    } yield
      new Creature {
        val creatureType: CreatureType = cType
        val health: Int                = hp
        val maxHealth: Int             = hp
        val stats: BaseStats           = baseStats

        val baseWeapon: Weapon    = wpn
        def weapon[_: RS]: Weapon = wpn

        val armour: Armour = armr

        val offHand: Option[Equipment] = optShield

        val armourClass: Int = armour.armourClass(stats.dexterity)

        val proficiencyBonus: ProficiencyBonus = profBonus

        def updateHealth(modification: Int): Creature =
          throw new NotImplementedError("Impossible to implement, results in recursive definition of Creature")
        val abilities: List[CreatureAbility] = List.empty // TODO add core abilities
        val resistances: List[DamageType]    = List.empty
        val immunities: List[DamageType]     = List.empty
        val name: String                     = n
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
        creature.armour,
        creature.offHand,
        0,
        creature.resistances,
        creature.immunities,
        List.empty, // TODO add core abilities
        creature.name
      )
  }

  implicit val arbFighterFightingStyle: Arbitrary[Seq[FighterFightingStyle]] = Arbitrary {
    Gen.someOf(Archery, Defense, Dueling, GreatWeaponFighting, Protection, TwoWeaponFighting)
  }

  implicit val arbFighterAbilityUsages: Arbitrary[FighterAbilities] = Arbitrary {
    for {
      secondWindUsed  <- Gen.oneOf(true, false)
      actionSurgeUsed <- Gen.oneOf(true, false)
    } yield FighterAbilities(secondWindUsed, actionSurgeUsed)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      abilityUsages      <- arbFighterAbilityUsages.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Fighter(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.baseWeapon,
        creature.armour,
        creature.offHand,
        fightingStyles.toList,
        abilityUsages,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        Fighter.standardFighterAbilities,
        creature.name
      )
  }

  implicit val arbChampion: Arbitrary[Champion] = Arbitrary {
    for {
      creature       <- arbCreature.arbitrary
      abilityUsages      <- arbFighterAbilityUsages.arbitrary
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
        creature.baseWeapon,
        armour,
        shield,
        fightingStyles.toList,
        abilityUsages,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        Champion.standardChampionAbilities,
        creature.name
      )
  }
}
