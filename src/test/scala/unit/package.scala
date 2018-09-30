import io.github.tjheslin1.classes.Fighter
import io.github.tjheslin1.model._
import org.scalacheck.{Arbitrary, Gen}

package object unit {

  def genBaseStats: Gen[BaseStats] =
    for {
      str <- Gen.choose(1, 20)
      dex <- Gen.choose(1, 20)
      con <- Gen.choose(1, 20)
      int <- Gen.choose(1, 20)
      wis <- Gen.choose(1, 20)
      cha <- Gen.choose(1, 20)
    } yield BaseStats(str, dex, con, int, wis, cha)

  def genWeapon: Gen[Weapon] =
    for {
      sides <- Gen.choose(1, 12)
    } yield
      new Weapon {
        def damage(implicit rollStrategy: RollStrategy): Int = Dice.defaultRandomiser(sides)
      }

  def genCreature: Gen[Creature] =
    for {
      health       <- Gen.choose(1, 100)
      stats        <- genBaseStats
      ac           <- Gen.choose(5, 25)
      weapon       <- genWeapon
      creatureType <- Gen.frequency((1, PlayerCharacter), (1, Monster))
    } yield Creature(health, stats, ac, 0, weapon, creatureType)

  def genFighter: Gen[Fighter] =
    for {
      creature <- genCreature
    } yield Fighter(creature.copy(creatureType = PlayerCharacter))

  def arbFighter: Arbitrary[Fighter] = Arbitrary(_ => genFighter)
}
