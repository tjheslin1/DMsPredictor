package io.github.tjheslin1.util

import io.github.tjheslin1.model.Dice

object IntOps {

  implicit class IntOps(val wrappable: Int) extends AnyVal {
    def *(dice: Dice) = dice.roll(wrappable)
  }
}
