package io.github.tjheslin1.dmspredictor.model

import cats.Show

abstract class Weapon {

  def name: String
  def damage(implicit rollStrategy: RollStrategy): Int
}

object Weapon {

  def apply(weaponName: String, dmg: Int) = new Weapon {
    override def damage(implicit rollStrategy: RollStrategy): Int = dmg
    override def name: String                                     = weaponName
  }

  implicit val weaponShow: Show[Weapon] = Show.show { weapon =>
    s"Weapon: ${weapon.name}"
  }
}
