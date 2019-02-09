package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait Player extends Creature {

  val level: Level
  val bonusActionUsed: Boolean

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

        case c: Barbarian => Barbarian._bonusActionUsed.set(bonusUsed)(c)
        case c: Berserker => Berserker._bonusActionUsed.set(bonusUsed)(c)
      }
  }
}
