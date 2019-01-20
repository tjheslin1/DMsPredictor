package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.ExtraAttack
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, attackAndDamage, resolveDamage}
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.util.IntOps._

/**
  * In terms of combat, most Combat Maneuvers add a superiority dice roll to damage, therefore only one is implemented.
  * The other maneuvers grant more speed, etc, which is outside the scope of the simulator.
  */
object BattleMasterAbilities {

  def disarmingAttackManeuver(combatant: Combatant): Ability = new Ability(combatant) {
    val battleMaster = combatant.creature.asInstanceOf[BattleMaster]

    val name                    = "Maneuver: Disarming Attack"
    val levelRequirement: Level = LevelThree

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = battleMaster.level >= levelRequirement && battleMaster.superiorityDiceCount > 0

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          if (creatureHasExtraAttackAbility(combatant)) {

            val (updatedAttacker, optUpdatedTarget) = disarmingAttack(combatant, target)

            (battleMaster.superiorityDiceCount - 1, optUpdatedTarget) match {
              case (_, None) => (updatedAttacker, none[Combatant])
              case (noDice @ 0, Some(updatedTarget)) =>
                nextAbilityToUseInConjunction(updatedAttacker, name).fold {
                  val (updatedRegularAttacker, regularAttackTarget) = attackAndDamage(updatedAttacker, updatedTarget)
                  (updatedRegularAttacker, regularAttackTarget.some)
                } {
                  case (_, ability) => useAdditionalAbility(ability, updatedAttacker, updatedTarget)
                }
              case (_, Some(updatedTarget)) => disarmingAttack(updatedAttacker, updatedTarget)
            }
          } else {
            disarmingAttack(combatant, target)
          }
      }
    }

    def update: Creature = battleMaster

    private def creatureHasExtraAttackAbility(combatantToCheck: Combatant): Boolean = {
      val extraAttack = CoreAbilities.extraAttack(combatantToCheck)

      combatantToCheck.creature.abilities
        .map {
          case (_, ability) => ability(combatantToCheck).name
        }
        .contains(ExtraAttack) &&
      battleMaster.level >= extraAttack.levelRequirement &&
      extraAttack.conditionMet && extraAttack.triggerMet
    }

    private def disarmingAttack[_: RS](attackingCombatant: Combatant,
                                       attackTarget: Combatant): (Combatant, Option[Combatant]) = {
      val attackResult = attack(attackingCombatant, attackingCombatant.creature.weapon, attackTarget)
      val (updatedAttacker, updatedTarget) =
        resolveDamage(attackingCombatant, attackTarget, attackResult, 1 * BattleMaster.SuperiorityDice)

      attackResult match {
        case Miss | CriticalMiss =>
          (updatedAttacker, updatedTarget.some)
        case Hit | CriticalHit =>
          val updatedSuperiorityDiceCountAttacker = updateSuperiorityDice(updatedAttacker)
          val targetCreature                      = updatedTarget.creature

          if ((D20.roll() + mod(targetCreature.stats.strength)) >= battleMaster.maneuverSaveDC)
            (updatedSuperiorityDiceCountAttacker, updatedTarget.some)
          else {
            val disarmedTarget = (Combatant.creatureLens composeLens Creature.creatureBaseWeaponLens)
              .set(UnarmedStrike(targetCreature))(updatedTarget)

            (updatedSuperiorityDiceCountAttacker, disarmedTarget.some)
          }
      }
    }

    private def updateSuperiorityDice(combatant: Combatant): Combatant = {
      val battleMaster = combatant.creature.asInstanceOf[BattleMaster]

      val updatedSuperiorityDiceCount = battleMaster.superiorityDiceCount - 1
      Combatant.creatureLens
        .set(BattleMaster._superiorityDiceCount.set(updatedSuperiorityDiceCount)(battleMaster))(combatant)
    }
  }
}
