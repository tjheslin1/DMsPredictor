package io.github.tjheslin1.dmspredictor.util

import io.github.tjheslin1.dmspredictor.model._

object IntOps {

  implicit class IntOps(val wrappable: Int) extends AnyVal {
    def *[_: RS](dice: Dice) = dice.roll(wrappable)
  }
}
