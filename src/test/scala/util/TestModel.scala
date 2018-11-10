package util

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.weapons.Shortsword

object TestModel {

  def player(): Creature = Creature(10, BaseStats(12, 12, 12, 12, 12, 12), 10, Shortsword, PlayerCharacter)
  def enemy(): Creature  = Creature(10, BaseStats(8, 8, 8, 8, 8, 8), 10, Shortsword, Monster)

  def guaranteedKillWeapon = new Weapon {
    def name = "guaranteed kill weapon"
    def damage(implicit rollStrategy: RollStrategy): Int = 1000
  }
}
