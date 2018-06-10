package io.github.tjheslin1.model

abstract class Creature {

  def health(implicit rollStrategy: RollStrategy): Int
  def stats: BaseStats
  def armourClass: Int
  def experience: Int
}
