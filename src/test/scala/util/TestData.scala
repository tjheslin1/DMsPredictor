package util

import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  val DamageTypes = List(Bludgeoning, Piercing, Slashing)

  case class TestMonster(health: Int,
                         stats: BaseStats,
                         armourClass: Int,
                         weapon: Weapon,
                         override val resistances: List[DamageType] = List(),
                         override val immunities: List[DamageType] = List(),
                         override val name: String = NameGenerator.randomName)
      extends Creature {

    val creatureType: CreatureType = Monster

    def updateHealth(modification: Int): Creature = copy(health = Math.max(health + modification, 0))
  }

  implicit class TestMonsterOps(val testMonster: TestMonster) extends AnyVal {
    def withName(creatureName: String)           = testMonster.copy(name = creatureName)
    def withHealth(hp: Int)                      = testMonster.copy(health = hp)
    def withStrength(strengthScore: Stat)        = testMonster.copy(stats = testMonster.stats.copy(strength = strengthScore))
    def withWeapon(wpn: Weapon)                  = testMonster.copy(weapon = wpn)
    def withResistance(creatureRes: DamageType*) = testMonster.copy(resistances = creatureRes.toList)
    def withImmunity(creatureImm: DamageType*)   = testMonster.copy(immunities = creatureImm.toList)
    def withNoResistances                        = testMonster.copy(resistances = List.empty)
    def withNoImmunities                         = testMonster.copy(immunities = List.empty)
    def withNoResistancesOrImmunities            = testMonster.copy(resistances = List.empty, immunities = List.empty)
    def withCombatIndex(index: Int)              = Combatant(index, testMonster)
  }

  implicit class FighterOps(val fighter: Fighter) extends AnyVal {
    def withName(creatureName: String)           = fighter.copy(name = creatureName)
    def withHealth(hp: Int)                      = fighter.copy(health = hp)
    def withStrength(strengthScore: Stat)        = fighter.copy(stats = fighter.stats.copy(strength = strengthScore))
    def withWeapon(wpn: Weapon)                  = fighter.copy(weapon = wpn)
    def withResistance(creatureRes: DamageType*) = fighter.copy(resistances = creatureRes.toList)
    def withImmunity(creatureImm: DamageType*)   = fighter.copy(immunities = creatureImm.toList)
    def withNoResistances                        = fighter.copy(resistances = List.empty)
    def withNoImmunities                         = fighter.copy(immunities = List.empty)
    def withNoResistancesOrImmunities            = fighter.copy(resistances = List.empty, immunities = List.empty)
    def withCombatIndex(index: Int)              = Combatant(index, fighter)
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

  implicit val arbLevel: Arbitrary[Level] = Arbitrary {
    Gen.oneOf(LevelOne, LevelTwo, LevelThree, LevelFour)
  }

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName       <- Gen.alphaStr
      weaponDamageType <- arbDamageType.arbitrary
      sides            <- Gen.choose(1, 12)
    } yield
      new Weapon {
        val name: String = weaponName
        val damageType   = weaponDamageType

        def damage(implicit rollStrategy: RollStrategy): Int = Dice.defaultRandomiser(sides)
      }
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      hp        <- Gen.choose(10, 80)
      baseStats <- arbBaseStats.arbitrary
      ac        <- Gen.choose(5, 25)
      wpn       <- arbWeapon.arbitrary
      cType     <- Gen.oneOf(PlayerCharacter, Monster)
      profBonus <- Gen.choose(0, 6)
    } yield
      new Creature {
        val creatureType: CreatureType     = cType
        val health: Int                    = hp
        val stats: BaseStats               = baseStats
        val armourClass: Int               = ac
        val weapon: Weapon                 = wpn
        override val proficiencyBonus: Int = profBonus

        def updateHealth(modification: Int): Creature =
          throw new NotImplementedError("Impossible to implement, results in recursive definition of Creature")
      }
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
      level    <- arbLevel.arbitrary
    } yield
      Fighter(
        level,
        creature.health,
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.weapon,
        secondWindUsed = false,
        creature.proficiencyBonus,
        creature.resistances,
        creature.immunities,
        creature.name
      )
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield
      TestMonster(
        creature.health,
        creature.stats,
        creature.armourClass,
        creature.weapon,
        creature.resistances,
        creature.immunities,
        creature.name
      )
  }
}
