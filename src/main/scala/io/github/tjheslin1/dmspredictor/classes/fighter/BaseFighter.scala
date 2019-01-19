package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait BaseFighter extends Product with Serializable {

  val level: Level
  val fightingStyles: List[FighterFightingStyle]
  val abilityUsages: BaseFighterAbilities
}

object BaseFighter {

  val abilityUsagesLens: Lens[BaseFighter, BaseFighterAbilities] =
    Lens[BaseFighter, BaseFighterAbilities](_.abilityUsages) { abilityUsages =>
      {
        case fighter: Fighter               => Fighter._abilityUsages.set(abilityUsages)(fighter)
        case champion: Champion             => Champion._abilityUsages.set(abilityUsages)(champion)
        case eldritchKnight: EldritchKnight => EldritchKnight._abilityUsages.set(abilityUsages)(eldritchKnight)
      }
    }
}
