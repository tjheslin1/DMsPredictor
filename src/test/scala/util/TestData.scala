package util

import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.dmspredictor.classes.Fighter
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

object TestData {

  case class TestMonster(creature: Creature)

  implicit class CreatureOps(val creature: Creature) extends AnyVal {
    def withName(creatureName: String) = creature.copy(name = creatureName)
    def withHealth(hp: Int) = creature.copy(health = hp)
    def withStrength(strengthScore: Stat) = creature.copy(stats = creature.stats.copy(strength = strengthScore))
    def withCombatIndex(index: Int) = Combatant(index, creature)
  }

  implicit class CombatantOps(val combatant: Combatant) extends AnyVal {
    def withName(combatantName: String) = combatant.copy(creature = combatant.creature.withName(combatantName))
    def withHealth(hp: Int) = combatant.copy(creature = combatant.creature.withHealth(hp))
    def withStrength(strengthScore: Stat) = combatant.copy(creature = combatant.creature.withStrength(strengthScore))
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

  implicit val arbLevel: Arbitrary[Level] = Arbitrary {
    Gen.oneOf(LevelOne, LevelTwo, LevelThree, LevelFour)
  }

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName <- Gen.alphaStr
      sides      <- Gen.choose(1, 12)
    } yield Weapon(weaponName, Dice.defaultRandomiser(sides))
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      health           <- Gen.choose(10, 80)
      stats            <- arbBaseStats.arbitrary
      ac               <- Gen.choose(5, 25)
      weapon           <- arbWeapon.arbitrary
      creatureType     <- Gen.oneOf(PlayerCharacter, Monster)
      proficiencyBonus <- Gen.choose(0, 6)
    } yield Creature(health, stats, ac, weapon, creatureType, proficiencyBonus)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
      level    <- arbLevel.arbitrary
    } yield Fighter(creature.copy(creatureType = PlayerCharacter), level)
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary
    } yield TestMonster(creature.copy(creatureType = Monster))
  }
}
