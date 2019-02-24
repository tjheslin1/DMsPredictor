package io.github.tjheslin1.dmspredictor.model

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
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

  def rollAttack[_: RS](attacker: Combatant, target: Combatant): Int =
    (attacker.creature.attackStatus, target.creature.defenseStatus) match {
      case (Advantage, Advantage)       => D20.roll()
      case (Disadvantage, Disadvantage) => D20.roll()
      case (Advantage, _)               => D20.rollWithAdvantage()
      case (_, Disadvantage)            => D20.rollWithAdvantage()
      case (Disadvantage, _)            => D20.rollWithDisadvantage()
      case (_, Advantage)               => D20.rollWithDisadvantage()
      case _                            => D20.roll()
    }

  def attack[_: RS](attacker: Combatant,
                    attackerWeapon: Weapon,
                    target: Combatant): AttackResult = {

    val roll = rollAttack(attacker, target)

    logger.debug(s"D20.roll() of $roll")

    if (attacker.creature.scoresCritical(roll)) CriticalHit
    else if (roll == 1) CriticalMiss
    else {
      val totalAttackRoll = attacker.creature match {
        case player: Player =>
          roll +
            mod(player.stats.strength) +
            attackerWeapon.hitBonus +
            player.proficiencyBonus
        case monster =>
          roll +
            mod(monster.stats.strength) +
            attackerWeapon.hitBonus
      }

      if (totalAttackRoll >= target.creature.armourClass) Hit else Miss
    }
  }

  def resolveDamageMainHand[_: RS](attacker: Combatant,
                                   target: Combatant,
                                   attackResult: AttackResult,
                                   damageBonus: Int = 0): (Combatant, Combatant) =
    resolveDamage(attacker, target, attacker.creature.weapon, attackResult, damageBonus)

  def resolveDamage[_: RS](attacker: Combatant,
                           target: Combatant,
                           weapon: Weapon,
                           attackResult: AttackResult,
                           damageBonus: Int = 0): (Combatant, Combatant) = {

    val dmg = Math.max(
      0,
      attackResult match {
        case CriticalHit =>
          (weapon.damage + weapon.damage) + mod(attacker.creature.stats.strength) + damageBonus
        case Hit          => weapon.damage + mod(attacker.creature.stats.strength) + damageBonus
        case Miss         => 0
        case CriticalMiss => 0
      }
    )

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, weapon.damageType, attackResult))

    (attacker, damagedTarget)
  }

  def attackAndDamage[_: RS](attacker: Combatant, target: Combatant): (Combatant, Combatant) = {
    val attackResult = attack(attacker, attacker.creature.weapon, target)

    if (attackResult.result > 0)
      resolveDamage(attacker, target, attacker.creature.weapon, attackResult)
    else {
      logger.debug(s"${attacker.creature.name} misses regular attack")
      (attacker, target)
    }
  }

  def attackAndDamageTimes[_: RS](times: Int,
                                  attacker: Combatant,
                                  target: Combatant): (Combatant, Combatant) =
    runCombatantTimes(times, attacker, target, attackAndDamage)

  def runCombatantTimes(
      times: Int,
      c1: Combatant,
      c2: Combatant,
      f: (Combatant, Combatant) => (Combatant, Combatant)): (Combatant, Combatant) =
    (1 to times).foldLeft[(Combatant, Combatant)]((c1, c2)) { (combatants, _) =>
      val (a, t) = combatants
      f(a, t)
    }
}
