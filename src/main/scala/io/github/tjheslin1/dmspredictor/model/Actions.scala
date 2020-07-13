package io.github.tjheslin1.dmspredictor.model

import cats.syntax.eq._
import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.ability.{OnWeaponDamage, OnWeaponDamageAbility}
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.PaladinSpells.blessAttackBonus
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

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

  def attack[_: RS](
      attacker: Combatant,
      attackerWeapon: Weapon,
      target: Combatant
  ): (AttackResult, Combatant) = {

    val roll = rollAttack(attacker, target)

    logger.debug(s"D20.roll() to attack of $roll")

    if (attacker.creature.scoresCritical(roll)) (CriticalHit, target)
    else if (roll == 1) (CriticalMiss, target)
    else {
      val totalAttackRoll = attacker.creature match {
        case player: Player =>
          val modifier = weaponModifier(attackerWeapon, player)

          roll + modifier +
            attackerWeapon.hitBonus +
            player.proficiencyBonus +
            blessAttackBonus(attacker.creature)
        case _ =>
          roll +
            attackerWeapon.hitBonus +
            blessAttackBonus(attacker.creature)
      }

      if (totalAttackRoll >= target.creature.armourClass) {
        target.creature.reactionOnHit.fold[(AttackResult, Combatant)]((Hit, target)) {
          onHitReaction =>
            if (target.creature.reactionUsed) (Hit, target)
            else {
              val (updatedAttackResult, updatedTarget) =
                onHitReaction.updateAttackOnReaction(target.creature, totalAttackRoll)

              val updatedTargetCombatant = Combatant.creatureLens.set(updatedTarget)(target)

              (updatedAttackResult, updatedTargetCombatant)
            }
        }
      } else (Miss, target)
    }
  }

  def resolveDamageMainHand[_: RS](
      attacker: Combatant,
      target: Combatant,
      others: List[Combatant],
      attackResult: AttackResult,
      damageBonus: Int = 0
  ): (Combatant, Combatant, List[Combatant]) =
    resolveDamage(
      attacker,
      target,
      others,
      attacker.creature.weapon,
      attackResult,
      damageBonus,
      addStatModifier = true
    )

  /**
    * @param attacker
    * @param target
    * @param others is the list of other combatants not including the `target`
    * @param weapon
    * @param attackResult
    * @param damageBonus
    * @param addStatModifier
    * @tparam _
    * @return
    */
  def resolveDamage[_: RS](
      attacker: Combatant,
      target: Combatant,
      others: List[Combatant],
      weapon: Weapon,
      attackResult: AttackResult,
      damageBonus: Int = 0,
      addStatModifier: Boolean = true
  ): (Combatant, Combatant, List[Combatant]) = {

    val onWeaponDamageAbility = availableOnWeaponDamageAction(attacker, target)
      .fold(none[OnWeaponDamageAbility])(ability => ability(attacker).some)

    val dmg = calculatedDamage(
      attacker,
      attackResult,
      weapon,
      onWeaponDamageAbility,
      damageBonus,
      addStatModifier
    )

    val updatedAttacker = Combatant.creatureLens.set(
      onWeaponDamageAbility.fold(attacker.creature)(ability => ability.update)
    )(attacker)

    val updatedTarget = target.creature.reactionOnDamage.fold {
      Combatant.creatureLens.set(
        target.creature.updateHealth(dmg, weapon.damageType, attackResult)
      )(target)
    } { reaction =>
      if (target.creature.reactionUsed) {
        Combatant.creatureLens.set(
          target.creature.updateHealth(dmg, weapon.damageType, attackResult)
        )(target)
      } else {
        logger.debug(s"${target.creature.name} used ${reaction.name} (reaction)")

        Combatant.creatureLens.set(
          reaction.updateHealthOnReaction(target.creature, dmg, weapon.damageType, attackResult)
        )(target)
      }
    }

    val (updatedAttacker2, updatedTarget2, updatedOthers) =
      (target.creature, updatedTarget.creature) match {
        case (concentratingSpellCaster: SpellCaster, damagedSpellCaster: SpellCaster)
            if lossOfConcentration(concentratingSpellCaster, damagedSpellCaster) =>
          updateConditionOnOthers(concentratingSpellCaster, updatedAttacker, updatedTarget, others)
        case _ =>
          (updatedAttacker, updatedTarget, others)
      }

    val conditionHandledTargetCreature =
      updatedTarget2.creature.conditions
        .filter(_.isHandledOnDamage)
        .foldLeft(updatedTarget2.creature) {
          case (creature, condition) => condition.handleOnDamage(creature, dmg)
        }

    val conditionHandledTarget =
      Combatant.creatureLens.set(conditionHandledTargetCreature)(updatedTarget2)

    (updatedAttacker2, conditionHandledTarget, updatedOthers)
  }

  def attackAndDamage[_: RS](
      attacker: Combatant,
      target: Combatant,
      others: List[Combatant]
  ): (Combatant, Combatant, List[Combatant]) = {
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
      others: List[Combatant]
  ): (Combatant, Combatant, List[Combatant]) =
    runCombatantTimes(times, attacker, target, others, attackAndDamage)

  def runCombatantTimes(
      times: Int,
      c1: Combatant,
      c2: Combatant,
      c3: List[Combatant],
      f: (Combatant, Combatant, List[Combatant]) => (Combatant, Combatant, List[Combatant])
  ): (Combatant, Combatant, List[Combatant]) =
    (1 to times).foldLeft[(Combatant, Combatant, List[Combatant])]((c1, c2, c3)) {
      (combatants, _) =>
        val (a, t, o) = combatants
        f(a, t, o)
    }

  private def updateConditionOnOthers[_: RS](
      concentratingSpellCaster: SpellCaster,
      updatedAttacker: Combatant,
      updatedTarget: Combatant,
      others: List[Combatant]
  ) = {
    concentratingSpellCaster.concentratingSpell.fold((updatedAttacker, updatedTarget, others)) {
      case conditionSpell: ConditionSpell =>
        val concentratedCondition = conditionSpell.conditionFrom(concentratingSpellCaster)

        // TODO Should remove condition from whoever was targeted by adding identifier to Condition
        val conditionRemovedAttacker = removeCondition(updatedAttacker, concentratedCondition)

        (
          conditionRemovedAttacker,
          updatedTarget,
          others.map(removeCondition(_, concentratedCondition)))
      case multiTargetBuffSpell: MultiTargetBuffSpell =>
        val concentratedBuffCondition = multiTargetBuffSpell.buffCondition

        val conditionRemovedTarget = removeCondition(updatedTarget, concentratedBuffCondition)

        val updatedOthers = others
          .map(removeCondition(_, concentratedBuffCondition))

        (updatedAttacker, conditionRemovedTarget, updatedOthers)
      case _ => (updatedAttacker, updatedTarget, others)
    }
  }

  private def calculatedDamage[_: RS](
      attacker: Combatant,
      attackResult: AttackResult,
      weapon: Weapon,
      onWeaponDamageAbility: Option[OnWeaponDamageAbility],
      damageBonus: Int,
      addStatModifier: Boolean
  ): Int = {
    val modifier = if (addStatModifier) weaponModifier(weapon, attacker.creature) else 0

    Math.max(
      0,
      attackResult match {
        case CriticalHit =>
          val doubleWeaponDamage = weapon.damage + weapon.damage

          val doubleOnWeaponAbilityDamage = onWeaponDamageAbility.fold(0) { onWeaponDamageAbility =>
            val abilityDamage = onWeaponDamageAbility.damage() + onWeaponDamageAbility.damage()

            logger.debug(
              s"${attacker.creature.name} rolls an extra $abilityDamage damage (critical hit) using ${onWeaponDamageAbility.name}"
            )

            abilityDamage
          }

          doubleWeaponDamage + doubleOnWeaponAbilityDamage + modifier + damageBonus
        case Hit =>
          val onWeaponAbilityDamage = onWeaponDamageAbility.fold(0) { onWeaponDamageAbility =>
            val abilityDamage = onWeaponDamageAbility.damage()

            logger.debug(
              s"${attacker.creature.name} rolls an extra $abilityDamage damage using ${onWeaponDamageAbility.name}"
            )

            abilityDamage
          }

          weapon.damage + modifier + damageBonus + onWeaponAbilityDamage
        case Miss         => 0
        case CriticalMiss => 0
      }
    )
  }

  private def weaponModifier(weapon: Weapon, creature: Creature): Int =
    weapon.weaponType match {
      case Melee if weapon.finesse =>
        Math.max(mod(creature.stats.strength), mod(creature.stats.dexterity))
      case Melee  => mod(creature.stats.strength)
      case Ranged => mod(creature.stats.dexterity)
    }

  private def lossOfConcentration(
      spellCaster: SpellCaster,
      updatedSpellCaster: SpellCaster
  ): Boolean =
    spellCaster.isConcentrating && updatedSpellCaster.isConcentrating == false

  private def removeCondition(combatant: Combatant, condition: Condition): Combatant =
    if (combatant.creature.conditions.exists(_ === condition)) {
      val remainingConditions = combatant.creature.conditions diff List(condition)

      (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
        .set(remainingConditions)(combatant)
    } else
      combatant

  private def availableOnWeaponDamageAction(
      attacker: Combatant,
      target: Combatant
  ): Option[Combatant => OnWeaponDamageAbility] =
    attacker.creature.abilities
      .sortBy(ability => ability(attacker).order)
      .find { combatantAbility =>
        combatantAbility(attacker) match {
          case ability: OnWeaponDamageAbility =>
            ability.abilityAction == OnWeaponDamage && ability.conditionMet &&
              ability.triggerOnSingleTargetMet(target)
          case _ => false
        }
      }
      .map(_.asInstanceOf[Combatant => OnWeaponDamageAbility])
}
