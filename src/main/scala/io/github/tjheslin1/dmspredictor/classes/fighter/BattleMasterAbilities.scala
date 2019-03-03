package io.github.tjheslin1.dmspredictor.classes.fighter

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, SingleAttack}
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

/**
  * In terms of combat, most Combat Maneuvers add a superiority dice roll to damage, therefore only one is implemented.
  * The other maneuvers grant more speed, etc, which is outside the scope of the simulator.
  */
object BattleMasterAbilities extends LazyLogging {

  def disarmingAttackManeuver(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val battleMaster = combatant.creature.asInstanceOf[BattleMaster]

      val name             = "Maneuver: Disarming Attack"
      val order            = currentOrder
      val levelRequirement = LevelThree
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]) = true

      def conditionMet: Boolean =
        battleMaster.level >= levelRequirement && battleMaster.superiorityDiceCount > 0

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used Disarming Attack")

        val enemies = monsters(others)
        val target  = nextToFocus(enemies, focus)

        target match {
          case None => (combatant, others)
          case Some(attackTarget: Combatant) =>
            val attackResult = attack(combatant, combatant.creature.weapon, attackTarget)
            val (updatedAttacker, updatedTarget) =
              resolveDamageMainHand(combatant,
                                    attackTarget,
                                    attackResult,
                                    1 * BattleMaster.SuperiorityDice)

            attackResult match {
              case Miss | CriticalMiss =>
                (updatedAttacker, others.replace(updatedTarget))
              case Hit | CriticalHit =>
                val targetCreature = updatedTarget.creature

                if (savingThrowPassed(battleMaster.maneuverSaveDC, Strength, targetCreature))
                  (updatedAttacker, others.replace(updatedTarget))
                else {
                  val disarmedTarget =
                    (Combatant.creatureLens composeLens Creature.creatureBaseWeaponLens)
                      .set(UnarmedStrike(targetCreature))(updatedTarget)

                  (updatedAttacker, others.replace(disarmedTarget))
                }
            }
        }
      }

      def update: Creature = {
        val updatedSuperiorityDiceCount = battleMaster.superiorityDiceCount - 1
        BattleMaster._superiorityDiceCount.set(updatedSuperiorityDiceCount)(battleMaster)
      }
    }
}
