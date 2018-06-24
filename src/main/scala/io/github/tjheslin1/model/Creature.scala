package io.github.tjheslin1.model

import io.github.tjheslin1.model.Modifier.mod
import io.github.tjheslin1.simulation.{Loss, SimulationStatus, Success}
import io.github.tjheslin1.util.NameGenerator

abstract class Creature {

  val name: String = NameGenerator.randomName

  def calculateHealth(implicit rollStrategy: RollStrategy): Int
  def stats: BaseStats
  def armourClass: Int
  def experience: Int

  def attack(mob: Creature)(implicit rollStrategy: RollStrategy): SimulationStatus =
    if (D20.roll() + mod(stats.strength) > mob.armourClass) Success else Loss

  def resolveDamage(weapon: Weapon, mob: Creature)(implicit rollStrategy: RollStrategy): (Creature, Creature) = {
    val dmg = weapon.damage

    if (dmg >= mob.calculateHealth) (this, mob) // TODO: return mob with adjusted health
    else (this, mob)
  }

  def weapon: Weapon
  def level: Level
}
