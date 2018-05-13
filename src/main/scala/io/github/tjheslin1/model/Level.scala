package io.github.tjheslin1.model

sealed trait Level {
  val lvl: Int
}

object LevelOne extends Level {
  val lvl = 1
}

object LevelTwo extends Level {
  val lvl = 2
}

object LevelThree extends Level {
  val lvl = 3
}

object LevelFour extends Level {
  val lvl = 4
}
