package io.github.tjheslin1.util

import io.github.tjheslin1.model.{Dice, RollStrategy}
import scala.Predef.{$conforms => _}

object IntOps {

  implicit class IntOps(val wrappable: Int) extends AnyVal {
    def *(dice: Dice)(implicit rollStrategy: RollStrategy) = dice.roll(wrappable)
  }
}
