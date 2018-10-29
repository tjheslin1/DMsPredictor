import com.danielasfregola.randomdatagenerator.magnolia.RandomDataGenerator
import eu.timepit.refined
import eu.timepit.refined.W
import eu.timepit.refined.numeric.Interval
import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model.BaseStats.Stat
import io.github.tjheslin1.model._
import io.github.tjheslin1.monsters.Goblin
import org.scalacheck.{Arbitrary, Gen}
import shapeless._

package object unit extends RandomDataGenerator {

  implicit val arbStat: Arbitrary[Stat] =
    Arbitrary {
      Gen
        .choose(1, 30)
        .map(refined.refineV[Interval.ClosedOpen[W.`1`.T, W.`31`.T]](_))
        .flatMap {
          case Right(i) => Gen.const(i)
          case Left(_) => Gen.fail
        }
    }

  implicit val arbBaseStats: Arbitrary[BaseStats] = cachedImplicit

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName <- Gen.alphaStr
      sides <- Gen.choose(1, 12)
    } yield Weapon(weaponName, Dice.defaultRandomiser(sides))
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      health <- Gen.choose(10, 80)
      stats <- arbBaseStats.arbitrary
      ac <- Gen.choose(5, 25)
      weapon <- arbWeapon.arbitrary
      creatureType <- Gen.oneOf(PlayerCharacter, Monster)
    } yield Creature(health, stats, ac, 0, weapon, creatureType)
  }

  implicit val arbFighter: Arbitrary[Fighter] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary suchThat (_.creatureType == PlayerCharacter)
    } yield Fighter(creature)
  }

  implicit val arbTestMonster: Arbitrary[TestMonster] = Arbitrary {
    for {
      creature <- arbCreature.arbitrary suchThat (_.creatureType == Monster)
    } yield TestMonster(creature)
  }

  case class TestMonster(creature: Creature)
}
