package io.github.tjheslin1.model

abstract class Creature {

  def stats: BaseStats
  def health: Int
  def armourClass: Int
  def experience: Int
}
