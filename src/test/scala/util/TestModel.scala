package util

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.weapons.Shortsword

object TestModel {

  def player(): Creature = Creature(10, BaseStats(12, 12, 12, 12, 12, 12), 10, Shortsword, PlayerCharacter, 2)
  def enemy(): Creature  = Creature(10, BaseStats(8, 8, 8, 8, 8, 8), 10, Shortsword, Monster)

  val guaranteedKillWeapon = Weapon.fixedDamageWeapon("guaranteed kill weapon", Magical, 1000)
}
