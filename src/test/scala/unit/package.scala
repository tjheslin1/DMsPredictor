import io.github.tjheslin1.model._
import org.scalacheck.{Arbitrary, Gen}

package object unit {

  implicit val arbBaseStats: Arbitrary[BaseStats] = Arbitrary {
    for {
      str <- Gen.choose(1, 20)
      dex <- Gen.choose(1, 20)
      con <- Gen.choose(1, 20)
      int <- Gen.choose(1, 20)
      wis <- Gen.choose(1, 20)
      cha <- Gen.choose(1, 20)
    } yield BaseStats(str, dex, con, int, wis, cha)
  }

  implicit val arbWeapon: Arbitrary[Weapon] = Arbitrary {
    for {
      weaponName <- Gen.alphaStr
      sides      <- Gen.choose(1, 12)
    } yield Weapon(weaponName, Dice.defaultRandomiser(sides))
  }

  implicit val arbCreature: Arbitrary[Creature] = Arbitrary {
    for {
      health       <- Gen.choose(10, 80)
      stats        <- arbBaseStats.arbitrary
      ac           <- Gen.choose(5, 25)
      weapon       <- arbWeapon.arbitrary
      creatureType <- Gen.oneOf(PlayerCharacter, Monster)
    } yield Creature(health, stats, ac, 0, weapon, creatureType)
  }
}
