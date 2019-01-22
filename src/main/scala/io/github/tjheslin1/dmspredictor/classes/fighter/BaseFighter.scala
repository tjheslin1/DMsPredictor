package io.github.tjheslin1.dmspredictor.classes.fighter

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model._
import monocle.Lens

trait BaseFighter extends Player with Product with Serializable {

  val fightingStyles: List[FighterFightingStyle]
  val abilityUsages: BaseFighterAbilities
}

object BaseFighter {

  val abilityUsagesLens: Lens[BaseFighter, BaseFighterAbilities] =
    Lens[BaseFighter, BaseFighterAbilities](_.abilityUsages) { abilityUsages =>
      {
        case battleMaster: BattleMaster     => BattleMaster._abilityUsages.set(abilityUsages)(battleMaster)
        case eldritchKnight: EldritchKnight => EldritchKnight._abilityUsages.set(abilityUsages)(eldritchKnight)
        case champion: Champion             => Champion._abilityUsages.set(abilityUsages)(champion)
        case fighter: Fighter               => Fighter._abilityUsages.set(abilityUsages)(fighter)
      }
    }
}
