package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.model.Level

trait Player extends Product with Serializable {

  val level: Level
  val bonusActionUsed: Boolean
}
