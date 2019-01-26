package io.github.tjheslin1.dmspredictor.classes.barbarian

import io.github.tjheslin1.dmspredictor.classes.Player
import monocle.Lens

trait BaseBarbarian extends Player with Product with Serializable {

  val inRage: Boolean
  val rageUsages: Int
}

object BaseBarbarian {

  val inRageLens: Lens[BaseBarbarian, Boolean] = Lens[BaseBarbarian, Boolean](_.inRage) { rage =>
    {
      case b: Barbarian => Barbarian._inRage.set(rage)(b)
    }
  }

  val rageUsagesLens: Lens[BaseBarbarian, Int] = Lens[BaseBarbarian, Int](_.rageUsages) { rageNum =>
    {
      case b: Barbarian => Barbarian._rageUsages.set(rageNum)(b)
    }
  }
}
