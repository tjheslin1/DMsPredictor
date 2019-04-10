package io.github.tjheslin1.dmspredictor.classes.rogue

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue.sneakAttackDamage
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.ListOps._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object BaseRogueAbilities extends LazyLogging {

  def sneakAttack(currentPriority: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name: String                 = "Sneak Attack"
    val order: Int                   = currentPriority
    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = WholeAction

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean =
      baseRogue.attackStatus == Advantage || baseRogue.hiddenFrom.isEmpty == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      nextToFocus(baseRogue.hiddenFrom, focus) match {
        case None => (combatant, others)
        case Some(target) =>
          val sneakAttackingRogue =
            (Combatant.creatureLens composeLens Creature.creatureAttackStatusLens)
              .set(Advantage)(combatant)

          attack(sneakAttackingRogue, sneakAttackingRogue.creature.weapon, target) match {
            case CriticalMiss | Miss => (combatant, others)
            case attackResult @ (CriticalHit | Hit) =>
              val sneakAttackDmg = sneakAttackDamage(baseRogue.level) * BaseRogue.SneakAttackDice

              val (updatedRogue, updatedTarget, updatedOthers) =
                resolveDamage(sneakAttackingRogue,
                              target,
                              others,
                              sneakAttackingRogue.creature.weapon,
                              attackResult,
                              sneakAttackDmg)

              logger.debug(s"${baseRogue.name} dealt $sneakAttackDmg sneak attack damage")

              (updatedRogue, updatedOthers.replace(updatedTarget))
          }

      }
    }

    def update: Creature = BaseRogue.hiddenFromLens.set(List.empty[Combatant])(baseRogue)
  }

  /**
    * Assumed to be using Cunning Action.
    */
  def hide(currentPriority: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name: String = "Hide (Rogue)"
    val order: Int   = currentPriority

    val levelRequirement: Level = LevelTwo

    val abilityAction: AbilityAction = BonusAction

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean                        = baseRogue.bonusActionUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      val hideDc = if (baseRogue.stealthProficiency) {
        D20.roll() + mod(baseRogue.stats.dexterity) + baseRogue.proficiencyBonus
      } else {
        D20.roll() + mod(baseRogue.stats.dexterity)
      }

      val updatedRogue = monsters(others).foldLeft(baseRogue) {
        case (hidingRogue, enemy) =>
          if (savingThrowPassed(hideDc, Wisdom, enemy.creature)) hidingRogue
          else {
            val updatedEnemiesHiddenFrom = hidingRogue.hiddenFrom ++ List(enemy)

            logger.debug(s"${baseRogue.name} is hidden from ${enemy.creature.name}")

            BaseRogue.hiddenFromLens.set(updatedEnemiesHiddenFrom)(baseRogue)
          }
      }

      val updatedCombatant = Combatant.creatureLens.set(updatedRogue)(combatant)

      (updatedCombatant, others)
    }

    def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseRogue)
  }

  def twoWeaponFighting(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name             = "Two Weapon Fighting"
    val order            = currentOrder
    val levelRequirement = LevelOne
    val abilityAction    = SingleAttack

    def triggerMet(others: List[Combatant]) = true

    def conditionMet: Boolean = combatant.creature.offHand match {
      case Some(w: Weapon) =>
        baseRogue.bonusActionUsed == false &&
          w.twoHanded == false &&
          combatant.creature.baseWeapon.twoHanded == false
      case _ => false
    }

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used two weapon fighting")

      nextToFocus(monsters(others), focus) match {
        case None => (combatant, others)
        case Some(attackTarget) =>
          val mainHandAttack = attack(combatant, combatant.creature.weapon, attackTarget)

          val (updatedAttacker, attackTarget1, updatedOthers) =
            if (mainHandAttack.result > 0)
              resolveDamageMainHand(combatant, attackTarget, others, mainHandAttack)
            else
              (combatant, attackTarget, others)

          val updatedEnemies = monsters(updatedOthers).replace(attackTarget1)

          nextToFocus(updatedEnemies, focus) match {
            case None => (combatant, updatedOthers)
            case Some(nextTarget) =>
              val offHandWeapon = combatant.creature.offHand.get.asInstanceOf[Weapon]
              val offHandAttack = attack(updatedAttacker, offHandWeapon, nextTarget)

              val (attacker2, attackTarget2, updatedOthers2) =
                if (offHandAttack.result > 0)
                  resolveDamage(updatedAttacker,
                                nextTarget,
                                updatedOthers,
                                offHandWeapon,
                                offHandAttack)
                else
                  (updatedAttacker, nextTarget, updatedOthers)

              (attacker2, updatedOthers2.replace(attackTarget2))
          }
      }
    }

    def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseRogue)
  }

}
