package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.Weapon.UnarmedStrike
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, SingleAttack}
import io.github.tjheslin1.dmspredictor.util.IntOps._

/**
  * In terms of combat, most Combat Maneuvers add a superiority dice roll to damage, therefore only one is implemented.
  * The other maneuvers grant more speed, etc, which is outside the scope of the simulator.
  */
object BattleMasterAbilities {

  def disarmingAttackManeuver(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val battleMaster = combatant.creature.asInstanceOf[BattleMaster]

      val name             = "Maneuver: Disarming Attack"
      val order            = currentOrder
      val levelRequirement = LevelThree
      val abilityAction    = SingleAttack

      val triggerMet: Boolean = true
      def conditionMet: Boolean =
        battleMaster.level >= levelRequirement && battleMaster.superiorityDiceCount > 0

      def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
        target match {
          case None => (combatant, none[Combatant])
          case Some(target: Combatant) =>
            val attackResult = attack(combatant, combatant.creature.weapon, target)
            val (updatedAttacker, updatedTarget) =
              resolveDamageMainHand(combatant,
                                    target,
                                    attackResult,
                                    1 * BattleMaster.SuperiorityDice)

            attackResult match {
              case Miss | CriticalMiss =>
                (updatedAttacker, updatedTarget.some)
              case Hit | CriticalHit =>
                val targetCreature = updatedTarget.creature

                if (strengthSavingThrowPassed(targetCreature))
                  (updatedAttacker, updatedTarget.some)
                else {
                  val disarmedTarget =
                    (Combatant.creatureLens composeLens Creature.creatureBaseWeaponLens)
                      .set(UnarmedStrike(targetCreature))(updatedTarget)

                  (updatedAttacker, disarmedTarget.some)
                }
            }
        }

      def update: Creature = {
        val updatedSuperiorityDiceCount = battleMaster.superiorityDiceCount - 1
        BattleMaster._superiorityDiceCount.set(updatedSuperiorityDiceCount)(battleMaster)
      }

      private def strengthSavingThrowPassed[_: RS](targetCreature: Creature) =
        (D20.roll() + mod(targetCreature.stats.strength)) >= battleMaster.maneuverSaveDC
    }
}
