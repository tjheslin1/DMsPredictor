package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.Creature.creatureHealthLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
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

  def secondWind(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Second Wind"
    val order            = currentOrder
    val levelRequirement = LevelTwo
    val abilityAction    = WholeAction

    def triggerMet   = combatant.creature.health <= combatant.creature.maxHealth / 2
    def conditionMet = baseFighter.level.value >= levelRequirement && baseFighter.abilityUsages.secondWindUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val updatedHealth =
        Math.min(combatant.creature.maxHealth, combatant.creature.health + (1 * HitDice) + baseFighter.level.value)
      val updatedCombatant = (Combatant.creatureLens composeLens creatureHealthLens).set(updatedHealth)(combatant)

      (updatedCombatant, None)
    }

    def update: Creature =
      (BaseFighter.abilityUsagesLens composeLens secondWindUsedLens).set(true)(baseFighter).asInstanceOf[Creature]
  }

  def twoWeaponFighting(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Two Weapon Fighting"
    val order            = currentOrder
    val levelRequirement = LevelOne
    val abilityAction    = BonusAction

    val triggerMet: Boolean = true

    def conditionMet: Boolean = combatant.creature.offHand match {
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
            if (mainHandAttack.result > 0) resolveDamageMainHand(combatant, target, mainHandAttack)
            else
              (combatant, target)

          val offHandWeapon = combatant.creature.offHand.get.asInstanceOf[Weapon]
          val offHandAttack = attack(attacker1, offHandWeapon, attackTarget1)

          val (attacker2, attackTarget2) =
            if (offHandAttack.result > 0) resolveDamage(attacker1, attackTarget1, offHandWeapon, offHandAttack)
            else
              (attacker1, attackTarget1)

          (attacker2, attackTarget2.some)
      }

    def update: Creature = combatant.creature
  }

  def actionSurge(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant: Combatant) {
    val baseFighter = combatant.creature.asInstanceOf[BaseFighter]

    val name             = "Action Surge"
    val order            = currentOrder
    val levelRequirement = LevelTwo
    val abilityAction    = WholeAction

    val triggerMet: Boolean   = true
    def conditionMet: Boolean = baseFighter.abilityUsages.actionSurgeUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case None => (combatant, none[Combatant])
        case Some(target: Combatant) =>
          nextAbilityToUseInConjunction(combatant, order, AbilityAction.any)
            .fold(useAttackActionTwice(combatant, target)) { nextAbility =>
              val (updatedAttacker, optUpdatedTarget) = useAdditionalAbility(nextAbility, combatant, target)

              optUpdatedTarget.fold((updatedAttacker, none[Combatant])) { updatedTarget =>
                nextAbilityToUseInConjunction(updatedAttacker, order, AbilityAction.any)
                  .fold(useAttackActionTwice(updatedAttacker, updatedTarget)) { nextAbility2 =>
                    useAdditionalAbility(nextAbility2, updatedAttacker, updatedTarget)
                  }
              }
            }
      }
    }

    def update: Creature =
      (BaseFighter.abilityUsagesLens composeLens actionSurgeUsedLens).set(true)(baseFighter).asInstanceOf[Creature]
  }
}
