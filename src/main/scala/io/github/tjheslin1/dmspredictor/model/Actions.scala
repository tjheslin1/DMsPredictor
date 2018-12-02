package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Modifier.mod

sealed trait AttackResult {
  def result: Int
}

case object CriticalHit extends AttackResult {
  val result = 2
}

case object Hit extends AttackResult {
  val result = 1
}
case object Miss extends AttackResult {
  val result = 0
}
case object CriticalMiss extends AttackResult {
  val result = -1
}

object Actions extends LazyLogging {

  def attack[_: RS](attacker: Creature, attackee: Creature): AttackResult = {
    val roll = D20.roll()

    if (roll == 20) CriticalHit
    else if (roll == 1) CriticalMiss
    else if (roll + mod(attacker.stats.strength) + attacker.proficiencyBonus >= attackee.armourClass) Hit
    else Miss
  }

  def resolveDamage[_: RS](attacker: Creature, attackee: Creature, attackResult: AttackResult): (Creature, Creature) = {

    val dmg = Math.max(
      0,
      attackResult match {
        case CriticalHit  => (attacker.weapon.damage + attacker.weapon.damage) + mod(attacker.stats.strength)
        case Hit          => attacker.weapon.damage
        case Miss         => 0
        case CriticalMiss => 0
      }
    )

    logger.debug(s"${attacker.name} attacks ${attackee.name} for $dmg damage")

    val damagedAttackee = attackee.copy(health = Math.max(attackee.health - dmg, 0))

    (attacker, damagedAttackee)
  }

  def attackAndDamage[_: RS](attacker: Creature, attackee: Creature) = {
    val attackResult = attack(attacker, attackee)

    if (attackResult.result > 0) resolveDamage(attacker, attackee, attackResult)
    else
      (attacker, attackee)
  }
}
