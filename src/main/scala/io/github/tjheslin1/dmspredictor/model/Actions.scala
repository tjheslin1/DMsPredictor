package io.github.tjheslin1.dmspredictor.model

import cats.syntax.eq._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.ApplyConditionSpell

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
        case _ =>
          roll + attackerWeapon.hitBonus
      }

      if (totalAttackRoll >= target.creature.armourClass) Hit else Miss
    }
  }

  def resolveDamageMainHand[_: RS](attacker: Combatant,
                                   target: Combatant,
                                   others: List[Combatant],
                                   attackResult: AttackResult,
                                   damageBonus: Int = 0): (Combatant, Combatant, List[Combatant]) =
    resolveDamage(attacker, target, others, attacker.creature.weapon, attackResult, damageBonus)

  def resolveDamage[_: RS](attacker: Combatant,
                           target: Combatant,
                           others: List[Combatant],
                           weapon: Weapon,
                           attackResult: AttackResult,
                           damageBonus: Int = 0): (Combatant, Combatant, List[Combatant]) = {

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

    val updated = Combatant.creatureLens.set(
      target.creature.updateHealth(dmg, weapon.damageType, attackResult))(target)

    val updatedOthers = (target.creature, updated.creature) match {
      case (spellCaster: SpellCaster, damagedSpellCaster: SpellCaster)
          if spellCaster.isConcentrating && damagedSpellCaster.isConcentrating == false =>
        spellCaster.concentratingSpell.fold(others) {
          case conditionSpell: ApplyConditionSpell =>
            val concentratedCondition: Condition = conditionSpell.conditionFrom(spellCaster)

            others.map { other =>
              if (other.creature.conditions.exists(_ === concentratedCondition)) {
                val remainingConditions = other.creature.conditions diff List(concentratedCondition)

                (Combatant.creatureLens composeLens Creature.creatureConditionsLens).set(remainingConditions)(other)
              } else
                other
            }
          case _ => others
        }
      case _ => others
    }

    val conditionHandledCreature =
      updated.creature.conditions.filter(_.handleOnDamage).foldLeft(updated.creature) {
        case (creature, condition) => condition.handleOnDamage(creature)
      }

    val conditionHandledTarget = Combatant.creatureLens.set(conditionHandledCreature)(updated)

    (attacker, conditionHandledTarget, updatedOthers)
  }

  def attackAndDamage[_: RS](attacker: Combatant,
                             target: Combatant,
                             others: List[Combatant]): (Combatant, Combatant, List[Combatant]) = {
    val attackResult = attack(attacker, attacker.creature.weapon, target)

    if (attackResult.result > 0)
      resolveDamage(attacker, target, others, attacker.creature.weapon, attackResult)
    else {
      logger.debug(s"${attacker.creature.name} misses regular attack")
      (attacker, target, others)
    }
  }

  def attackAndDamageTimes[_: RS](
      times: Int,
      attacker: Combatant,
      target: Combatant,
      others: List[Combatant]): (Combatant, Combatant, List[Combatant]) =
    runCombatantTimes(times, attacker, target, others, attackAndDamage)

  def runCombatantTimes(
      times: Int,
      c1: Combatant,
      c2: Combatant,
      c3: List[Combatant],
      f: (Combatant, Combatant, List[Combatant]) => (Combatant, Combatant, List[Combatant]))
    : (Combatant, Combatant, List[Combatant]) =
    (1 to times).foldLeft[(Combatant, Combatant, List[Combatant])]((c1, c2, c3)) {
      (combatants, _) =>
        val (a, t, o) = combatants
        f(a, t, o)
    }
}
