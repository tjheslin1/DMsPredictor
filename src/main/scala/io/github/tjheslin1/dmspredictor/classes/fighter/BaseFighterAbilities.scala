package io.github.tjheslin1.dmspredictor.classes.fighter

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter.BaseFighter.HitDice
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureHealthLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._
import monocle.Lens
import monocle.macros.GenLens

case class BaseFighterAbilities(secondWindUsed: Boolean, actionSurgeUsed: Boolean)

object BaseFighterAbilities extends LazyLogging {

  val secondWindUsedLens: Lens[BaseFighterAbilities, Boolean] =
    GenLens[BaseFighterAbilities](_.secondWindUsed)
  val actionSurgeUsedLens: Lens[BaseFighterAbilities, Boolean] =
    GenLens[BaseFighterAbilities](_.actionSurgeUsed)

  def allUsed(): BaseFighterAbilities   = BaseFighterAbilities(true, true)
  def allUnused(): BaseFighterAbilities = BaseFighterAbilities(false, false)

  def secondWind(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Second Wind"
    val order            = currentOrder
    val levelRequirement = LevelTwo
    val abilityAction    = BonusAction

    def triggerMet(others: List[Combatant]) =
      combatant.creature.health <= combatant.creature.maxHealth / 2

    def conditionMet =
      baseFighter.level.value >= levelRequirement && baseFighter.abilityUsages.secondWindUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used Second wind")

      val updatedHealth =
        Math.min(combatant.creature.maxHealth,
                 combatant.creature.health + (1 * HitDice) + baseFighter.level.value)
      val updatedCombatant =
        (Combatant.creatureLens composeLens creatureHealthLens).set(updatedHealth)(combatant)

      (updatedCombatant, others)
    }

    def update: Creature = {
      val secondWindUsedFighter = (BaseFighter.abilityUsagesLens composeLens secondWindUsedLens)
        .set(true)(baseFighter)

      Player.playerBonusActionUsedLens.set(true)(secondWindUsedFighter)
    }
  }

  def twoWeaponFighting(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Two Weapon Fighting"
    val order            = currentOrder
    val levelRequirement = LevelOne
    val abilityAction    = SingleAttack

    def triggerMet(others: List[Combatant]) = true

    def conditionMet: Boolean = combatant.creature.offHand match {
      case Some(w: Weapon) =>
        baseFighter.bonusActionUsed == false &&
          w.twoHanded == false &&
          combatant.creature.baseWeapon.twoHanded == false &&
          baseFighter.fightingStyles.contains(TwoWeaponFighting)
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

    def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseFighter)
  }

  def actionSurge(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant: Combatant) {
      val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

      val name             = "Action Surge"
      val order            = currentOrder
      val levelRequirement = LevelTwo
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]) = true
      def conditionMet: Boolean               = baseFighter.abilityUsages.actionSurgeUsed == false

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used Action Surge")

        nextAbilityToUseInConjunction(combatant, others, order, AbilityAction.Any)
          .fold(useAttackActionTwice(combatant, others, focus)) { nextAbility =>
            val (updatedAttacker, updatedOthers) =
              useAdditionalAbility(nextAbility, combatant, others, focus)

            nextAbilityToUseInConjunction(updatedAttacker, updatedOthers, order, AbilityAction.Any)
              .fold {
                nextToFocus(updatedOthers, focus).fold((updatedAttacker, updatedOthers)) {
                  nextTarget =>
                    val (updatedAttacker2, updatedTarget2, updatedOthers2) =
                      attackAndDamage(updatedAttacker, nextTarget, updatedOthers)

                    (updatedAttacker2, updatedOthers2.replace(updatedTarget2))
                }
              } { nextAbility2 =>
                useAdditionalAbility(nextAbility2, updatedAttacker, updatedOthers, focus)
              }
          }
      }

      def update: Creature =
        (BaseFighter.abilityUsagesLens composeLens actionSurgeUsedLens)
          .set(true)(baseFighter)
          .asInstanceOf[Creature]
    }
}
