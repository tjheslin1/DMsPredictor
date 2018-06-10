package io.github.tjheslin1.model

abstract class Weapon {

  def damage(implicit rollStrategy: RollStrategy): Int
}
