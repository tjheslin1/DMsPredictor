package io.github.tjheslin1.dmspredictor.model

import cats.syntax.eq._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.ConcentrationConditionSpell

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
                    target: Combatant): (AttackResult, Combatant) = {

    val roll = rollAttack(attacker, target)

    logger.debug(s"D20.roll() of $roll")

    if (attacker.creature.scoresCritical(roll)) (CriticalHit, target)
    else if (roll == 1) (CriticalMiss, target)
    else {
      val totalAttackRoll = attacker.creature match {
        case player: Player =>
          val modifier = weaponModifier(attackerWeapon, player)

          roll + modifier +
            attackerWeapon.hitBonus +
            player.proficiencyBonus
        case _ =>
          roll + attackerWeapon.hitBonus
      }

      if (totalAttackRoll >= target.creature.armourClass) {

        target.creature.reactionOnHit.fold[(AttackResult, Combatant)]((Hit, target)) {
          onHitReaction =>
            val (updatedAttackResult, updatedTarget) =
              onHitReaction.updateAttackOnReaction(target.creature, totalAttackRoll)

            val updatedTargetCombatant = Combatant.creatureLens.set(updatedTarget)(target)

            (updatedAttackResult, updatedTargetCombatant)
        }
      } else (Miss, target)
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

    val modifier = weaponModifier(weapon, attacker.creature)

    val dmg = Math.max(
      0,
      attackResult match {
        case CriticalHit =>
          (weapon.damage + weapon.damage) + modifier + damageBonus
        case Hit          => weapon.damage + modifier + damageBonus
        case Miss         => 0
        case CriticalMiss => 0
      }
    )

    val updatedTarget = target.creature.reactionOnDamage.fold {
      Combatant.creatureLens.set(
        target.creature.updateHealth(dmg, weapon.damageType, attackResult))(target)
    } { reaction =>
      logger.debug(s"${target.creature.name} used ${reaction.name} (reaction)")

      Combatant.creatureLens.set(
        reaction.updateHealthOnReaction(target.creature, dmg, weapon.damageType, attackResult))(
        target)
    }

    val (updatedAttacker, updatedOthers) = (target.creature, updatedTarget.creature) match {
      case (spellCaster: SpellCaster, damagedSpellCaster: SpellCaster)
          if lossOfConcentration(spellCaster, damagedSpellCaster) == false =>
        spellCaster.concentratingSpell.fold((attacker, others)) {
          case conditionSpell: ConcentrationConditionSpell =>
            val concentratedCondition: Condition = conditionSpell.conditionFrom(spellCaster)

            val conditionRemovedAttacker = removeCondition(attacker, concentratedCondition)

            (conditionRemovedAttacker, others.map(removeCondition(_, concentratedCondition)))
          case _ => (attacker, others)
        }
      case _ =>
        (attacker, others)
    }

    val conditionHandledCreature =
      updatedTarget.creature.conditions.filter(_.handleOnDamage).foldLeft(updatedTarget.creature) {
        case (creature, condition) => condition.handleOnDamage(creature)
      }

    val conditionHandledTarget = Combatant.creatureLens.set(conditionHandledCreature)(updatedTarget)

    (updatedAttacker, conditionHandledTarget, updatedOthers)
  }

  def attackAndDamage[_: RS](attacker: Combatant,
                             target: Combatant,
                             others: List[Combatant]): (Combatant, Combatant, List[Combatant]) = {
    val (attackResult, updatedTarget) = attack(attacker, attacker.creature.weapon, target)

    if (attackResult.result > 0)
      resolveDamage(attacker, updatedTarget, others, attacker.creature.weapon, attackResult)
    else {
      logger.debug(s"${attacker.creature.name} misses regular attack")
      (attacker, updatedTarget, others)
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

  private def weaponModifier(weapon: Weapon, creature: Creature): Int =
    if (weapon.finesse) {
      Math.max(mod(creature.stats.strength), mod(creature.stats.dexterity))
    } else
      mod(creature.stats.strength)

  private def lossOfConcentration(spellCaster: SpellCaster,
                                  updatedSpellCaster: SpellCaster): Boolean =
    spellCaster.isConcentrating && updatedSpellCaster.isConcentrating

  private def removeCondition(combatant: Combatant, condition: Condition): Combatant =
    if (combatant.creature.conditions.exists(_ === condition)) {
      val remainingConditions = combatant.creature.conditions diff List(condition)

      (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
        .set(remainingConditions)(combatant)
    } else
      combatant
}
