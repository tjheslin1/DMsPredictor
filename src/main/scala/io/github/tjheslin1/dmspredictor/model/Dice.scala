package io.github.tjheslin1.dmspredictor.model

import scala.util.Random

sealed trait Dice {

  def sides: Int

  def max: Int = sides

  def *(times: Int)(implicit rollStrategy: RollStrategy): Int = roll(times)(rollStrategy)

  def roll(times: Int = 1)(implicit rollStrategy: RollStrategy): Int =
    (1 to times).map(_ => rollStrategy(sides)).sum
}

object Dice {

  val defaultRandomiser: RollStrategy = sides => RollResult(Random.nextInt(sides) + 1)
  val naturalOne: RollStrategy        = _ => RollResult(1)

  def midpointRoundedUp(dice: Dice): Int = Math.ceil(1 + (dice.sides / 2)).toInt
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
  val sides                       = 20
  val naturalTwenty: RollStrategy = _ => RollResult(20)
}

object D100 extends Dice {
  val sides = 100
}
