package io.github.tjheslin1.model

import scala.util.Random

sealed trait Dice {

  val sides: Int

  def roll(times: Int): Int = {
    (1 to times).foldLeft(0)((acc, roll) => {
      println("roll = " + roll)
      acc + Random.nextInt(sides)
    })
  }
}

case object D4 extends Dice {

  val sides = 4

  def result(times: Int): Int = roll(times)
}