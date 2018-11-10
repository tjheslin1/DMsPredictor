package io.github.tjheslin1.dmspredictor.util

import io.github.tjheslin1.dmspredictor.model.{Dice, RollStrategy}

object IntOps {

  implicit class IntOps(val wrappable: Int) extends AnyVal {
    def *(dice: Dice)(implicit rollStrategy: RollStrategy) = dice.roll(wrappable)
  }
}
