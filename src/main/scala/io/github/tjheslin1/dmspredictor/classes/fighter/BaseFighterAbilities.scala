package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, attackAndDamageTimes, resolveDamage}
import io.github.tjheslin1.dmspredictor.model.Creature.creatureHealthLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.util.IntOps._
import monocle.Lens
import monocle.macros.GenLens

case class BaseFighterAbilities(secondWindUsed: Boolean, actionSurgeUsed: Boolean)

object BaseFighterAbilities {

  import Fighter._

  val secondWindUsedLens: Lens[BaseFighterAbilities, Boolean]  = GenLens[BaseFighterAbilities](_.secondWindUsed)
  val actionSurgeUsedLens: Lens[BaseFighterAbilities, Boolean] = GenLens[BaseFighterAbilities](_.actionSurgeUsed)

  def allUsed(): BaseFighterAbilities   = BaseFighterAbilities(true, true)
  def allUnused(): BaseFighterAbilities = BaseFighterAbilities(false, false)

  def secondWind(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Second Wind"
    val levelRequirement = LevelTwo
    val triggerMet       = combatant.creature.health <= combatant.creature.maxHealth / 2
    val conditionMet     = baseFighter.level.value >= levelRequirement && baseFighter.abilityUsages.secondWindUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val updatedHealth =
        Math.min(combatant.creature.maxHealth, combatant.creature.health + (1 * HitDice) + baseFighter.level.value)
      val updatedCombatant = (Combatant.creatureLens composeLens creatureHealthLens).set(updatedHealth)(combatant)

      (updatedCombatant, None)
    }

    def update: Creature =
      (BaseFighter.abilityUsagesLens composeLens secondWindUsedLens).set(true)(baseFighter).asInstanceOf[Creature]
  }

  def twoWeaponFighting(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name                    = "Two Weapon Fighting"
    val levelRequirement: Level = LevelOne
    val triggerMet: Boolean     = true
    val conditionMet: Boolean = combatant.creature.offHand match {
      case Some(w: Weapon) =>
        w.twoHanded == false && combatant.creature.baseWeapon.twoHanded == false && baseFighter.fightingStyles.contains(
          TwoWeaponFighting)
      case _ => false
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          val mainHandAttack = attack(combatant, combatant.creature.weapon, target)

          val (attacker1, attackTarget1) =
            if (mainHandAttack.result > 0) resolveDamage(combatant, target, mainHandAttack)
            else
              (combatant, target)

          val offHandAttack = attack(attacker1, combatant.creature.offHand.get.asInstanceOf[Weapon], attackTarget1)

          val (attacker2, attackTarget2) =
            if (offHandAttack.result > 0) resolveDamage(attacker1, attackTarget1, offHandAttack)
            else
              (attacker1, attackTarget1)

          (attacker2, attackTarget2.some)
      }

    def update: Creature = combatant.creature
  }

  def actionSurge(combatant: Combatant): Ability = new Ability(combatant: Combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name                    = "Action Surge"
    val levelRequirement: Level = LevelTwo
    val triggerMet: Boolean     = true
    val conditionMet: Boolean   = baseFighter.abilityUsages.actionSurgeUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          nextAbilityToUseInConjuction(combatant).fold {
            val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)

            (updatedAttacker, updatedTarget.some)
          } {
            case (_, ability) =>
              val (updatedAttacker, optUpdatedTarget) = {
                val (attackerUsedAbility, targetOfAbility) = ability(combatant).useAbility(target.some)
                val updatedAttackingCreature               = ability(attackerUsedAbility).update
                (combatant.copy(creature = updatedAttackingCreature), targetOfAbility)
              }

              optUpdatedTarget.fold((updatedAttacker, none[Combatant])) { updatedTarget =>
                nextAbilityToUseInConjuction(updatedAttacker).fold {
                  val (updatedAttacker2, updatedTarget2) = attackAndDamageTimes(2, updatedAttacker, updatedTarget)

                  (updatedAttacker2, updatedTarget2.some)
                } {
                  case (_, ability2) =>
                    val (attackerUsedAbility2, targetOfAbility2) = ability2(updatedAttacker).useAbility(updatedTarget.some)
                    val updatedAttackingCreature2 = ability(attackerUsedAbility2).update

                    (updatedAttacker.copy(creature = updatedAttackingCreature2), targetOfAbility2)
                }
              }
          }
      }
    }

    def update: Creature =
      (BaseFighter.abilityUsagesLens composeLens actionSurgeUsedLens).set(true)(baseFighter).asInstanceOf[Creature]

    private def nextAbilityToUseInConjuction(attacker: Combatant): Option[(Int, Combatant => Ability)] = {
      // Currently Action Surge will choose Two Weapon Fighting over Extra Attack
      attacker.creature.abilities.sortBy { case (priority, _) => priority }.find {
        case (_, creatureAbility) =>
          val ability = creatureAbility(attacker)
          ability.name != name && ability.conditionMet && ability.triggerMet
      }
    }
  }
}
