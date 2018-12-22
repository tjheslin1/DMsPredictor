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

  def attack[_: RS](attacker: Combatant, attackerWeapon: Weapon, target: Combatant): AttackResult = {
    val roll = D20.roll()

    if (roll == 20) CriticalHit
    else if (roll == 1) CriticalMiss
    else {
      val totalAttackRoll = roll +
        mod(attacker.creature.stats.strength) +
        attackerWeapon.hitBonus +
        attacker.creature.proficiencyBonus

      if (totalAttackRoll >= target.creature.armourClass) Hit else Miss
    }
  }

  def resolveDamage[_: RS](attacker: Combatant,
                           target: Combatant,
                           attackResult: AttackResult): (Combatant, Combatant) = {

    val dmg = Math.max(
      0,
      attackResult match {
        case CriticalHit =>
          (attacker.creature.weapon.damage + attacker.creature.weapon.damage) + mod(attacker.creature.stats.strength)
        case Hit          => attacker.creature.weapon.damage
        case Miss         => 0
        case CriticalMiss => 0
      }
    )

    val adjustedDamage = attacker.creature.weapon.damageType match {
      case damageType if target.creature.resistances.contains(damageType) => math.floor(dmg / 2).toInt
      case damageType if target.creature.immunities.contains(damageType)  => 0
      case _                                                              => dmg
    }

    logger.debug(s"${attacker.creature.name} attacks ${target.creature.name} for $adjustedDamage damage")

    val damagedTarget = target.copy(creature = target.creature.updateHealth(Math.negateExact(adjustedDamage)))

    (attacker, damagedTarget)
  }

  def attackAndDamage[_: RS](attacker: Combatant, target: Combatant) = {
    val attackResult = attack(attacker, attacker.creature.weapon, target)

    if (attackResult.result > 0) resolveDamage(attacker, target, attackResult)
    else
      (attacker, target)
  }
}
