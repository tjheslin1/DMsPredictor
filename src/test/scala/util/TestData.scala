package util

import cats.syntax.option._
import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.fighter.FighterAbilities.allUnused
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{NoArmour, Shield}
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  val DamageTypes = List(Bludgeoning, Piercing, Slashing)

  case class TestMonster(health: Int,
                         maxHealth: Int,
                         stats: BaseStats,
                         armourClass: Int,
                         wpn: Weapon,
                         abilities: List[CreatureAbility] = List.empty,
                         resistances: List[DamageType] = List(),
                         immunities: List[DamageType] = List(),
                         name: String = NameGenerator.randomName)
      extends Creature {

    val creatureType: CreatureType = EnemyMonster
    val proficiencyBonus: Int      = 0

    def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))

    def weapon[_: RS]: Weapon = wpn
  }

  implicit class TestMonsterOps(val testMonster: TestMonster) extends AnyVal {
    def withName(creatureName: String)           = testMonster.copy(name = creatureName)
    def withHealth(hp: Int)                      = testMonster.copy(health = hp)
    def withMaxHealth(hp: Int)                   = testMonster.copy(maxHealth = hp)
    def withStrength(strengthScore: Stat)        = testMonster.copy(stats = testMonster.stats.copy(strength = strengthScore))
    def withWeapon(weapon: Weapon)               = testMonster.copy(wpn = weapon)
    def withArmourClass(ac: Int)                 = testMonster.copy(armourClass = ac)
    def withResistance(creatureRes: DamageType*) = testMonster.copy(resistances = creatureRes.toList)
    def withImmunity(creatureImm: DamageType*)   = testMonster.copy(immunities = creatureImm.toList)
    def withNoResistances()                      = testMonster.copy(resistances = List.empty)
    def withNoImmunities()                       = testMonster.copy(immunities = List.empty)
    def withNoResistancesOrImmunities()          = testMonster.copy(resistances = List.empty, immunities = List.empty)

    def withAbilities(ablts: List[CreatureAbility]) = testMonster.copy(abilities = ablts)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    def withLevel(lvl: Level)                            = fighter.copy(level = lvl)
    def withName(creatureName: String)                   = fighter.copy(name = creatureName)
    def withHealth(hp: Int)                              = fighter.copy(health = hp)
    def withMaxHealth(hp: Int)                           = fighter.copy(maxHealth = hp)
    def withAllAbilitiesUsed()                           = fighter.copy(abilityUsages = FighterAbilities.allUsed())
    def withAllAbilitiesUnused()                         = fighter.copy(abilityUsages = allUnused())
    def withStrength(strengthScore: Stat)                = fighter.copy(stats = fighter.stats.copy(strength = strengthScore))
    def withDexterity(dexScore: Stat)                    = fighter.copy(stats = fighter.stats.copy(dexterity = dexScore))
    def withConstitution(conScore: Stat)                 = fighter.copy(stats = fighter.stats.copy(constitution = conScore))
    def withWeapon(weapon: Weapon)                       = fighter.copy(baseWeapon = weapon)
    def withArmour(armr: Armour)                         = fighter.copy(armour = armr)
    def withNoArmour()                                   = fighter.copy(armour = NoArmour)
    def withShield()                                     = fighter.copy(offHand = Shield().some)
    def withOffHand(equipment: Equipment)                = fighter.copy(offHand = equipment.some)
    def withNoShield()                                   = fighter.copy(offHand = None)
    def withResistance(creatureRes: DamageType*)         = fighter.copy(resistances = creatureRes.toList)
    def withImmunity(creatureImm: DamageType*)           = fighter.copy(immunities = creatureImm.toList)
    def withNoResistances()                              = fighter.copy(resistances = List.empty)
    def withNoImmunities()                               = fighter.copy(immunities = List.empty)
    def withNoResistancesOrImmunities()                  = fighter.copy(resistances = List.empty, immunities = List.empty)
    def withFightingStyle(styles: FighterFightingStyle*) = fighter.copy(fightingStyles = styles.toList)
    def withNoFightingStyles()                           = fighter.copy(fightingStyles = List.empty)
  }

  implicit class PlayerOps[T <: Creature](val t: T) extends AnyVal {
    def withCombatIndex(index: Int) = Combatant(index, t)
  }

  implicit class CombatantOps(val combatant: Combatant) extends AnyVal {
    def withCreature(c: Creature) = combatant.copy(creature = c)
  }
}

trait TestData extends RandomDataGenerator {

  import TestData._

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
      armour    <- arbArmour.arbitrary
      cType     <- Gen.oneOf(PlayerCharacter, EnemyMonster)
      profBonus <- Gen.choose(0, 6)
    } yield
      new Creature {
        val creatureType: CreatureType = cType
        val health: Int                = hp
        val stats: BaseStats           = baseStats
        def weapon[_: RS]: Weapon      = wpn
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
      armour         <- arbArmour.arbitrary
      shield         <- arbShield.arbitrary
      fightingStyles <- arbFighterFightingStyle.arbitrary
      level          <- arbLevel.arbitrary
    } yield
      Fighter(
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
