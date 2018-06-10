package io.github.tjheslin1.model

import io.github.tjheslin1.model.Modifier.mod
import io.github.tjheslin1.simulation._

import scala.Predef.{$conforms => _}

abstract class PlayerCharacter extends Creature {

  def weapon: Weapon

  def attack(mob: Creature)(implicit rollStrategy: RollStrategy): SimulationStatus =
    if (D20.roll() + mod(stats.strength) > mob.armourClass) Success else Loss

  def resolveDamage(mob: Creature)(implicit rollStrategy: RollStrategy) =
    if (weapon.damage >= mob.health) Success else Loss

  def level: Level
}
