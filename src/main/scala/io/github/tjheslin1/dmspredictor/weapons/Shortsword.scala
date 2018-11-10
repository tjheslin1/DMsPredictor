package io.github.tjheslin1.dmspredictor.weapons

import io.github.tjheslin1.dmspredictor.model.{D6, RollStrategy, Weapon}
import io.github.tjheslin1.dmspredictor.util.IntOps._

case object Shortsword extends Weapon {

  def name                                             = "Shortsword"
  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6

}
