package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities.ExtraAttack
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, resolveDamage}
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

    val name = "Maneuver: Disarming Attack"
    val levelRequirement: Level = LevelThree

    val triggerMet: Boolean   = true
    val conditionMet: Boolean = battleMaster.level >= levelRequirement && battleMaster.superiorityDiceCount > 0

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case None                    => (combatant, None)
        case Some(target: Combatant) =>
          // attack with bonus damage
          val extraAttack = CoreAbilities.extraAttack(combatant)
          if (creatureHasExtraAttackAbility(extraAttack)) {

            // target makes Strength saving throw or loses weapon (swap with unarmed)

            ???
          } else {
            val attackResult = attack(combatant, combatant.creature.weapon, target)
            val (updatedAttacker, updatedTarget) =
              resolveDamage(combatant, target, attackResult, 1 * BattleMaster.SuperiorityDice)

            // target makes Strength saving throw or loses weapon (swap with unarmed)
            attackResult match {
              case CriticalMiss | Miss => (updatedAttacker, updatedTarget.some)
              case CriticalHit | Hit =>
                val targetCreature = target.creature

                if ((D20.roll() + mod(targetCreature.stats.strength)) >=  battleMaster.maneuverSaveDC)
                  (updatedAttacker, updatedTarget.some)
                else {
                  val disarmedTarget = (Combatant.creatureLens composeLens Creature.creatureBaseWeaponLens).set(UnarmedStrike(targetCreature))(target)

                  (updatedAttacker, disarmedTarget.some)
                }
            }
          }
      }
    }

    def update: Creature = {
      val updatedSuperiorityDiceCount = battleMaster.superiorityDiceCount - 1
      BattleMaster._superiorityDiceCount.set(updatedSuperiorityDiceCount)(battleMaster)
    }

    private def creatureHasExtraAttackAbility(extraAttack: Ability): Boolean = {
      combatant.creature.abilities
        .map {
          case (_, ability) => ability(combatant).name
        }
        .contains(ExtraAttack) &&
      battleMaster.level >= extraAttack.levelRequirement &&
      extraAttack.conditionMet && extraAttack.triggerMet
    }
  }
}
