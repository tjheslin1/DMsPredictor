package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import monocle.Lens
import monocle.macros.GenLens

sealed trait SpellSlot extends Product with Serializable {
  val spellLevel: SpellLevel
  val count: Int
}

case class FirstLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 1
}

case class SecondLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 2
}

case class ThirdLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 3
}

case class FourthLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 4
}

case class FifthLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 5
}

case class SixthLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 6
}

case class SeventhLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 7
}

case class EighthLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 8
}

case class NinthLevelSpellSlots(count: Int) extends SpellSlot {
  val spellLevel: SpellLevel = 9
}

case class SpellSlots(
    firstLevel: FirstLevelSpellSlots,
    secondLevel: SecondLevelSpellSlots,
    thirdLevel: ThirdLevelSpellSlots,
    fourthLevel: FourthLevelSpellSlots,
    fifthLevel: FifthLevelSpellSlots,
    sixthLevel: SixthLevelSpellSlots,
    seventhLevel: SeventhLevelSpellSlots,
    eighthLevel: EighthLevelSpellSlots,
    ninthLevel: NinthLevelSpellSlots
)

object SpellSlots {

  // Update when Players are implemented above level five
  def apply(firstLevelSlots: Int, secondLevelSlots: Int, thirdLevelSlots: Int): SpellSlots =
    SpellSlots(
      FirstLevelSpellSlots(firstLevelSlots),
      SecondLevelSpellSlots(secondLevelSlots),
      ThirdLevelSpellSlots(thirdLevelSlots),
      FourthLevelSpellSlots(0),
      FifthLevelSpellSlots(0),
      SixthLevelSpellSlots(0),
      SeventhLevelSpellSlots(0),
      EighthLevelSpellSlots(0),
      NinthLevelSpellSlots(0)
    )

  def apply(
      firstLevelSlots: Int,
      secondLevelSlots: Int,
      thirdLevelSlots: Int,
      fourthLevelSlots: Int,
      fifthLevelSlots: Int,
      sixthLevelSlots: Int,
      seventhLevelSlots: Int,
      eigthLevelSlots: Int,
      ninthLevelSlots: Int
  ): SpellSlots =
    SpellSlots(
      FirstLevelSpellSlots(firstLevelSlots),
      SecondLevelSpellSlots(secondLevelSlots),
      ThirdLevelSpellSlots(thirdLevelSlots),
      FourthLevelSpellSlots(fourthLevelSlots),
      FifthLevelSpellSlots(fifthLevelSlots),
      SixthLevelSpellSlots(sixthLevelSlots),
      SeventhLevelSpellSlots(seventhLevelSlots),
      EighthLevelSpellSlots(eigthLevelSlots),
      NinthLevelSpellSlots(ninthLevelSlots)
    )

  // format: off
  def highestSpellSlotAvailable(spellSlots: SpellSlots): Option[SpellSlot] = spellSlots match {
    case SpellSlots(_, _, _, _, _, _, _, _, ninthLevel @ NinthLevelSpellSlots(count)) if count > 0 => ninthLevel.some
    case SpellSlots(_, _, _, _, _, _, _, eighthLevel @ EighthLevelSpellSlots(count), _) if count > 0 => eighthLevel.some
    case SpellSlots(_, _, _, _, _, _, seventhLevel @ SeventhLevelSpellSlots(count), _, _) if count > 0 => seventhLevel.some
    case SpellSlots(_, _, _, _, _, sixthLevel @ SixthLevelSpellSlots(count), _, _, _) if count > 0 => sixthLevel.some
    case SpellSlots(_, _, _, _, fifthLevel @ FifthLevelSpellSlots(count), _, _, _, _) if count > 0 => fifthLevel.some
    case SpellSlots(_, _, _, fourthLevel @ FourthLevelSpellSlots(count), _, _, _, _, _) if count > 0 => fourthLevel.some
    case SpellSlots(_, _, thirdLevel @ ThirdLevelSpellSlots(count), _, _, _, _, _, _) if count > 0 => thirdLevel.some
    case SpellSlots(_, secondLevel @ SecondLevelSpellSlots(count), _, _, _, _, _, _, _) if count > 0 =>
      secondLevel.some
    case SpellSlots(firstLevel @ FirstLevelSpellSlots(count), _, _, _, _, _, _, _, _) if count > 0 => firstLevel.some

    case _ => none[SpellSlot]
  }
  // format: on

  def spellSlotFromLevel(spellCaster: SpellCaster, spellLevel: SpellLevel): SpellSlot =
    spellLevel.value match {
      case 1 => spellCaster.spellSlots.firstLevel
      case 2 => spellCaster.spellSlots.secondLevel
      case 3 => spellCaster.spellSlots.thirdLevel
      case 4 => spellCaster.spellSlots.fourthLevel
      case 5 => spellCaster.spellSlots.fifthLevel
      case 6 => spellCaster.spellSlots.sixthLevel
      case 7 => spellCaster.spellSlots.seventhLevel
      case 8 => spellCaster.spellSlots.eighthLevel
      case 9 => spellCaster.spellSlots.ninthLevel
    }

  val firstLevelSpellSlotLens: Lens[SpellSlots, FirstLevelSpellSlots] =
    GenLens[SpellSlots](_.firstLevel)

  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlots, Int] =
    GenLens[FirstLevelSpellSlots](_.count)

  val firstLevelLens: Lens[SpellSlots, Int] =
    firstLevelSpellSlotLens composeLens firstLevelSpellSlotCountLens

  val secondLevelSpellSlotLens: Lens[SpellSlots, SecondLevelSpellSlots] =
    GenLens[SpellSlots](_.secondLevel)

  val secondLevelSpellSlotCountLens: Lens[SecondLevelSpellSlots, Int] =
    GenLens[SecondLevelSpellSlots](_.count)

  val secondLevelLens: Lens[SpellSlots, Int] =
    secondLevelSpellSlotLens composeLens secondLevelSpellSlotCountLens

  val thirdLevelSpellSlotLens: Lens[SpellSlots, ThirdLevelSpellSlots] =
    GenLens[SpellSlots](_.thirdLevel)

  val thirdLevelSpellSlotCountLens: Lens[ThirdLevelSpellSlots, Int] =
    GenLens[ThirdLevelSpellSlots](_.count)

  val thirdLevelLens: Lens[SpellSlots, Int] =
    thirdLevelSpellSlotLens composeLens thirdLevelSpellSlotCountLens

  val fourthLevelSpellSlotLens: Lens[SpellSlots, FourthLevelSpellSlots] =
    GenLens[SpellSlots](_.fourthLevel)

  val fourthLevelSpellSlotCountLens: Lens[FourthLevelSpellSlots, Int] =
    GenLens[FourthLevelSpellSlots](_.count)

  val fourthLevelLens: Lens[SpellSlots, Int] =
    fourthLevelSpellSlotLens composeLens fourthLevelSpellSlotCountLens

  val fifthLevelSpellSlotLens: Lens[SpellSlots, FifthLevelSpellSlots] =
    GenLens[SpellSlots](_.fifthLevel)

  val fifthLevelSpellSlotCountLens: Lens[FifthLevelSpellSlots, Int] =
    GenLens[FifthLevelSpellSlots](_.count)

  val fifthLevelLens: Lens[SpellSlots, Int] =
    fifthLevelSpellSlotLens composeLens fifthLevelSpellSlotCountLens

  val sixthLevelSpellSlotLens: Lens[SpellSlots, SixthLevelSpellSlots] =
    GenLens[SpellSlots](_.sixthLevel)

  val sixthLevelSpellSlotCountLens: Lens[SixthLevelSpellSlots, Int] =
    GenLens[SixthLevelSpellSlots](_.count)

  val sixthLevelLens: Lens[SpellSlots, Int] =
    sixthLevelSpellSlotLens composeLens sixthLevelSpellSlotCountLens

  val seventhLevelSpellSlotLens: Lens[SpellSlots, SeventhLevelSpellSlots] =
    GenLens[SpellSlots](_.seventhLevel)

  val seventhLevelSpellSlotCountLens: Lens[SeventhLevelSpellSlots, Int] =
    GenLens[SeventhLevelSpellSlots](_.count)

  val seventhLevelLens: Lens[SpellSlots, Int] =
    seventhLevelSpellSlotLens composeLens seventhLevelSpellSlotCountLens

  val eighthLevelSpellSlotLens: Lens[SpellSlots, EighthLevelSpellSlots] =
    GenLens[SpellSlots](_.eighthLevel)

  val eighthLevelSpellSlotCountLens: Lens[EighthLevelSpellSlots, Int] =
    GenLens[EighthLevelSpellSlots](_.count)

  val eighthLevelLens: Lens[SpellSlots, Int] =
    eighthLevelSpellSlotLens composeLens eighthLevelSpellSlotCountLens

  val ninthLevelSpellSlotLens: Lens[SpellSlots, NinthLevelSpellSlots] =
    GenLens[SpellSlots](_.ninthLevel)

  val ninthLevelSpellSlotCountLens: Lens[NinthLevelSpellSlots, Int] =
    GenLens[NinthLevelSpellSlots](_.count)

  val ninthLevelLens: Lens[SpellSlots, Int] =
    ninthLevelSpellSlotLens composeLens ninthLevelSpellSlotCountLens
}
