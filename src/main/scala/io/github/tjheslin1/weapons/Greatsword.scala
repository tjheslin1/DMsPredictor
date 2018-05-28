package io.github.tjheslin1.weapons

import io.github.tjheslin1.model.{D6, Weapon}
import io.github.tjheslin1.util.IntOps._

case object Greatsword extends Weapon {

  def damage: Int = 2 * D6

}
