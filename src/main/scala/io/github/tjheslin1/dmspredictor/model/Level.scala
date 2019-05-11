package io.github.tjheslin1.dmspredictor.model

sealed trait Level {
  val value: Int
}

object Level {
  def apply(value: Int): Level = value match {
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

object LevelTwenty extends Level {
  val value = 20
}
