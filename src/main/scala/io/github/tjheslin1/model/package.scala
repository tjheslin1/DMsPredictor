package io.github.tjheslin1

package object model {

  case class RollResult(result: Int)

  type RollStrategy = Int => RollResult

  implicit def rollResultConversion(rollResult: RollResult): Int = rollResult.result
  implicit def rollResultConversion(roll: Int): RollResult = RollResult(roll)

  implicit def levelConversion(level: Level): Int = level.lvl

  implicit val numericRollResult = new Numeric[RollResult] {
    def plus(x: RollResult, y: RollResult): RollResult  = x + y
    def minus(x: RollResult, y: RollResult): RollResult = x - y
    def times(x: RollResult, y: RollResult): RollResult = x * y
    def negate(x: RollResult): RollResult               = RollResult(-x.result)
    def fromInt(x: Int): RollResult                     = RollResult(x)
    def toInt(x: RollResult): Int                       = x.result
    def toLong(x: RollResult): Long                     = x.result
    def toFloat(x: RollResult): Float                   = x.result
    def toDouble(x: RollResult): Double                 = x.result
    def compare(x: RollResult, y: RollResult): Int = {
      if (x.result < y.result) -1
      else if (x.result == y.result) 0
      else 1
    }
  }
}
