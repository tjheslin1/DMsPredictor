package io.github.tjheslin1.dmspredictor.model

sealed trait Level {
  val value: Int
}

object Level {

  def apply(value: Int): Level =
    value match {
      case 1  => LevelOne
      case 2  => LevelTwo
      case 3  => LevelThree
      case 4  => LevelFour
      case 5  => LevelFive
      case 20 => LevelTwenty
    }
}

object LevelOne extends Level {
  val value = 1
}

object LevelTwo extends Level {
  val value = 2
}

object LevelThree extends Level {
  val value = 3
}

object LevelFour extends Level {
  val value = 4
}

object LevelFive extends Level {
  val value = 5
}

object LevelSix extends Level {
  val value = 6
}

object LevelSeven extends Level {
  val value = 7
}

object LevelEight extends Level {
  val value = 8
}

object LevelNine extends Level {
  val value = 9
}

object LevelTen extends Level {
  val value = 10
}

object LevelEleven extends Level {
  val value = 11
}

object LevelTwelve extends Level {
  val value = 12
}

object LevelThirteen extends Level {
  val value = 13
}

object LevelFourteen extends Level {
  val value = 14
}

object LevelFifteen extends Level {
  val value = 15
}

object LevelSixteen extends Level {
  val value = 16
}

object LevelSeventeen extends Level {
  val value = 17
}

object LevelEighteen extends Level {
  val value = 18
}

object LevelNineteen extends Level {
  val value = 19
}

object LevelTwenty extends Level {
  val value = 20
}
