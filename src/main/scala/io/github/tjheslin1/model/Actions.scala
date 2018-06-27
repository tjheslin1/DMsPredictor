package io.github.tjheslin1.model

import io.github.tjheslin1.model.Modifier.mod

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

object Actions {

  def attack(attacker: Creature, attackee: Creature)(implicit rollStrategy: RollStrategy): AttackResult = {
    val roll = D20.roll()

    if (roll == 20) CriticalHit
    else if (roll == 1) CriticalMiss
    else if (roll + mod(attacker.stats.strength) + attacker.proficiencyBonus > attackee.armourClass) Hit
    else Miss
  }

  def resolveDamage(attacker: Creature, attackee: Creature, attackResult: AttackResult)(
      implicit rollStrategy: RollStrategy): (Creature, Creature) = {

    val dmg = attackResult match {
      case CriticalHit  => attacker.weapon.damage * 2 + mod(attacker.stats.strength)
      case Hit          => attacker.weapon.damage
      case Miss         => 0
      case CriticalMiss => 0
    }

    val damagedAttackee = attackee.copy(health = Math.max(attackee.health - dmg, 0))

    (attacker, damagedAttackee)
  }

  def attackAndDamage(attacker: Creature, attackee: Creature)(implicit rollStrategy: RollStrategy) = {
    val attackResult = attack(attacker, attackee)
    if (attackResult.result > 0) resolveDamage(attacker, attackee, attackResult)
    else (attacker, attackee)
  }
}
