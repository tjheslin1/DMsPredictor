package io.github.tjheslin1.weapons

import io.github.tjheslin1.model.{D6, RollStrategy, Weapon}
import io.github.tjheslin1.util.IntOps._

case object Greatsword extends Weapon {

  def name                                             = "Greatsword"
  def damage(implicit rollStrategy: RollStrategy): Int = 2 * D6

}
