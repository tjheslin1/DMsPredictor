package io.github.tjheslin1.model

object Modifier {

  def mod(stat: Stat): Int = modifier(stat.score)

  val modifier = Map(
    1  -> -5,
    10 -> 0,
    2  -> -4,
    11 -> 0,
    3  -> -4,
    12 -> 1,
    4  -> -3,
    13 -> 1,
    5  -> -3,
    14 -> 2,
    6  -> -2,
    15 -> 2,
    7  -> -2,
    16 -> 3,
    8  -> -1,
    17 -> 3,
    9  -> -1,
    18 -> 4,
    20 -> 5,
    21 -> 5,
    22 -> 6,
    23 -> 6,
    24 -> 7,
    25 -> 7,
    26 -> 8,
    27 -> 8,
    28 -> 9,
    29 -> 9,
    30 -> 10
  )
}
