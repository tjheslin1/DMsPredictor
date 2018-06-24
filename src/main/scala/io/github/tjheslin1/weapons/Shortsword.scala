package io.github.tjheslin1.weapons

import io.github.tjheslin1.model.{D6, RollStrategy, Weapon}
import io.github.tjheslin1.util.IntOps._

import scala.Predef.{$conforms => _}

case object Shortsword extends Weapon {

  def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6

}
