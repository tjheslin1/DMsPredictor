package io.github.tjheslin1.dmspredictor.classes.barbarian

import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian.resetStatus
import io.github.tjheslin1.dmspredictor.model.Creature
import monocle.Lens

trait BaseBarbarian extends Player with Product with Serializable {

  val inRage: Boolean
  val rageUsages: Int
  val rageTurnsLeft: Int

  def turnReset(): Creature = resetStatus(this)
}

object BaseBarbarian {

  def resetStatus(baseBarbarian: BaseBarbarian): BaseBarbarian = ???

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

  val rageTurnsLeftLens: Lens[BaseBarbarian, Int] = Lens[BaseBarbarian, Int](_.rageTurnsLeft) { turnsLeft =>
    {
      case b: Barbarian => Barbarian._rageTurnsLeft.set(turnsLeft)(b)
    }
  }
}
