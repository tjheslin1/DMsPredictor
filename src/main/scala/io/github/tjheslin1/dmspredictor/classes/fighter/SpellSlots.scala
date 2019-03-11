package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import monocle.{Lens, Optional}
import monocle.macros.GenLens

case class SpellSlots(firstLevel: FirstLevelSpellSlots,
                      secondLevel: SecondLevelSpellSlots,
                      thirdLevel: ThirdLevelSpellSlots)

object SpellSlots {

  def highestSpellSlotAvailable(spellSlots: SpellSlots): Option[SpellSlot] = spellSlots match {
    case SpellSlots(_, _, thirdLevel @ ThirdLevelSpellSlots(count)) if count > 0 => thirdLevel.some
    case SpellSlots(_, secondLevel @ SecondLevelSpellSlots(count), _) if count > 0 =>
      secondLevel.some
    case SpellSlots(firstLevel @ FirstLevelSpellSlots(count), _, _) if count > 0 => firstLevel.some
    case _                                                                       => none[SpellSlot]
  }

  val firstLevelSpellSlotLens: Lens[SpellSlots, FirstLevelSpellSlots] =
    GenLens[SpellSlots](_.firstLevel)

  val firstLevelSpellSlotCountLens: Lens[FirstLevelSpellSlots, Int] =
    GenLens[FirstLevelSpellSlots](_.count)

  val firstLevelLens
    : Lens[SpellSlots, Int] = firstLevelSpellSlotLens composeLens firstLevelSpellSlotCountLens

  val secondLevelSpellSlotLens: Lens[SpellSlots, SecondLevelSpellSlots] =
    GenLens[SpellSlots](_.secondLevel)

  val secondLevelSpellSlotCountLens: Lens[SecondLevelSpellSlots, Int] =
    GenLens[SecondLevelSpellSlots](_.count)

  val secondLevelLens
    : Lens[SpellSlots, Int] = secondLevelSpellSlotLens composeLens secondLevelSpellSlotCountLens

  val thirdLevelSpellSlotLens: Lens[SpellSlots, ThirdLevelSpellSlots] =
    GenLens[SpellSlots](_.thirdLevel)

  val thirdLevelSpellSlotCountLens: Lens[ThirdLevelSpellSlots, Int] =
    GenLens[ThirdLevelSpellSlots](_.count)

  val thirdLevelLens
    : Lens[SpellSlots, Int] = thirdLevelSpellSlotLens composeLens thirdLevelSpellSlotCountLens
}
