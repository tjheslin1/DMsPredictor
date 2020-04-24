package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging

object HandleDamage extends LazyLogging {

  def adjustedDamage(dmg: Int, damageType: DamageType, creature: Creature): Int = {
    val adjustedDmg = damageType match {
      case dt if creature.damageResistances.contains(dt) => math.max(1, math.floor(dmg / 2).toInt)
      case dt if creature.damageImmunities.contains(dt)  => 0
      case _                                             => dmg
    }

    logger.debug(s"${creature.name} took $adjustedDmg (adjusted) $damageType damage")
    adjustedDmg
  }

  def applyDamage(creature: Creature, damage: Int): Creature = {
    val updatedHealth   = creature.health - damage
    val updatedCreature = Creature.creatureHealthLens.set(Math.max(0, updatedHealth))(creature)

    if (updatedHealth <= Math.negateExact(creature.maxHealth))
      Creature.creatureIsAliveLens.set(false)(updatedCreature)
    else
      updatedCreature
  }
}
