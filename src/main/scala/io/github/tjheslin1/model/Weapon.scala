package io.github.tjheslin1.model

import cats.Show

abstract class Weapon {

  def name: String
  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def apply(name: String, damage: Int) = new Weapon {
    def damage(implicit rollStrategy: RollStrategy): Int = damage
    def name: String = name
  }

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name}"
  }
}
