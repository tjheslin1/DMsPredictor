package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging

object AdjustedDamage extends LazyLogging {

  def adjustedDamage(dmg: Int, damageType: DamageType, creature: Creature): Int = {
    val adjustedDmg = damageType match {
      case dt if creature.resistances.contains(dt) => math.max(1, math.floor(dmg / 2).toInt)
      case dt if creature.immunities.contains(dt)  => 0
      case _                                       => dmg
    }

    logger.debug(s"${creature.name} took $adjustedDmg (adjusted) $damageType damage")
    adjustedDmg
  }
}
