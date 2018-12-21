package io.github.tjheslin1.dmspredictor.model

sealed trait Level {
  val value: Int
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

object LevelTwenty extends Level {
  val value = 20
}
