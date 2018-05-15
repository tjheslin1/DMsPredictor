package io.github.tjheslin1.model

import io.github.tjheslin1.simulation._

abstract class PlayerCharacter extends Creature {

  def weapon: Weapon
  def attack(mob: Creature): SimulationStatus = if (weapon.damage >= mob.health) Win else Loss
  def level: Level
}
