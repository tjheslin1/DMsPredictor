package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.{Spell, SpellLevel}
import monocle.Lens

trait Player extends Creature {

  val level: Level
  val bonusActionUsed: Boolean

  val cantripKnown: Option[Spell]                 = None
  val spellsKnown: Option[Map[SpellLevel, Spell]] = None
  val spellSlots: Option[SpellSlots]              = None

  val creatureType: CreatureType = PlayerCharacter
}

object Player {

  val playerBonusActionUsedLens: Lens[Player, Boolean] = Lens[Player, Boolean](_.bonusActionUsed) {
    bonusUsed =>
      {
        case c: BattleMaster   => BattleMaster._bonusActionUsed.set(bonusUsed)(c)
        case c: EldritchKnight => EldritchKnight._bonusActionUsed.set(bonusUsed)(c)
        case c: Champion       => Champion._bonusActionUsed.set(bonusUsed)(c)
        case c: Fighter        => Fighter._bonusActionUsed.set(bonusUsed)(c)

        case c: Barbarian    => Barbarian._bonusActionUsed.set(bonusUsed)(c)
        case c: Berserker    => Berserker._bonusActionUsed.set(bonusUsed)(c)
        case c: TotemWarrior => TotemWarrior._bonusActionUsed.set(bonusUsed)(c)

        case c: Cleric => Cleric._bonusActionUsed.set(bonusUsed)(c)

        case _ =>
          throw new NotImplementedError(
            "Missing playerBonusActionUsedLens lens for your new implementation of Player!")
      }
  }

  val spellSlotsLens: Lens[Player, SpellSlots] = Lens[Player, SpellSlots](_.spellSlots) {
    spellSlots =>
      {
        case c: EldritchKnight => EldritchKnight._spellSlots.set(spellSlots)(c)
        case c: Cleric         => Cleric._spellSlots.set(spellSlots)(c)
      }
  }
}
