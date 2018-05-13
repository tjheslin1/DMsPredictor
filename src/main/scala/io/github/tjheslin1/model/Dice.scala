package io.github.tjheslin1.model

import scala.util.Random

sealed trait Dice {

  def sides: Int

  def roll(times: Int): Int = {
    (1 to times).map(_ => Random.nextInt(sides-1) + 1).sum
  }

  def *(times: Int): Int = roll(times)
}

object D4 extends Dice {
  val sides = 4
}

object D6 extends Dice {
  val sides = 6
}

object D8 extends Dice {
  val sides = 8
}

object D10 extends Dice {
  val sides = 10
}

object D12 extends Dice {
  val sides = 12
}

object D20 extends Dice {
  val sides = 20
}

object D100 extends Dice {
  val sides = 100
}