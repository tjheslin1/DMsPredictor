package io.github.tjheslin1.dmspredictor.classes.fighter

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities
import io.github.tjheslin1.dmspredictor.model.Actions.{attack, attackAndDamageTimes, resolveDamage}
import io.github.tjheslin1.dmspredictor.model.Creature.creatureHealthLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability
import io.github.tjheslin1.dmspredictor.util.IntOps._
import monocle.Lens
import monocle.macros.GenLens

case class FighterAbilities(secondWindUsed: Boolean, actionSurgeUsed: Boolean)

object FighterAbilities {

  import Fighter._

  val secondWindUsedLens: Lens[FighterAbilities, Boolean]  = GenLens[FighterAbilities](_.secondWindUsed)
  val actionSurgeUsedLens: Lens[FighterAbilities, Boolean] = GenLens[FighterAbilities](_.actionSurgeUsed)

  def allUsed(): FighterAbilities   = FighterAbilities(true, true)
  def allUnused(): FighterAbilities = FighterAbilities(false, false)

  def secondWind(combatant: Combatant): Ability = new Ability(combatant) {
    val fighter = combatant.creature.asInstanceOf[Fighter]

    val levelRequirement = LevelTwo
    val triggerMet       = combatant.creature.health <= combatant.creature.maxHealth / 2
    val conditionMet     = fighter.level.value >= levelRequirement && fighter.abilityUsages.secondWindUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      val updatedHealth =
        Math.min(combatant.creature.maxHealth, combatant.creature.health + (1 * HitDice) + fighter.level.value)
      val updatedCombatant = (Combatant.creatureLens composeLens creatureHealthLens).set(updatedHealth)(combatant)

      (updatedCombatant, None)
    }

    def update: Fighter = (_abilityUsages composeLens secondWindUsedLens).set(true)(fighter)
  }

  def twoWeaponFighting(combatant: Combatant): Ability = new Ability(combatant) {
    val fighter = combatant.creature.asInstanceOf[Fighter]

    val levelRequirement: Level = LevelOne
    val triggerMet: Boolean     = true
    val conditionMet: Boolean = fighter.offHand match {
      case Some(w: Weapon) =>
        w.twoHanded == false && fighter.baseWeapon.twoHanded == false && fighter.fightingStyles.contains(
          TwoWeaponFighting)
      case _ => false
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case Some(target: Combatant) =>
          val mainHandAttack = attack(combatant, fighter.weapon, target)

          val (attacker1, attackTarget1) =
            if (mainHandAttack.result > 0) resolveDamage(combatant, target, mainHandAttack)
            else
              (combatant, target)

          val offHandAttack = attack(attacker1, fighter.offHand.get.asInstanceOf[Weapon], attackTarget1)

          val (attacker2, attackTarget2) =
            if (offHandAttack.result > 0) resolveDamage(attacker1, attackTarget1, offHandAttack)
            else
              (attacker1, attackTarget1)

          (attacker2, attackTarget2.some)
        case None => (combatant, None)
      }

    def update: Fighter = fighter
  }

  def actionSurge(combatant: Combatant): Ability = new Ability(combatant: Combatant) {
    val fighter = combatant.creature.asInstanceOf[Fighter]

    val levelRequirement: Level = LevelTwo
    val triggerMet: Boolean     = true
    val conditionMet: Boolean   = fighter.abilityUsages.actionSurgeUsed == false

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      target match {
        case Some(target: Combatant) =>
          val extraAttack = CoreAbilities.extraAttack(combatant)
          if (fighter.level.value >= extraAttack.levelRequirement &&
              extraAttack.conditionMet && extraAttack.triggerMet) {
            val (_, updatedTarget)                 = extraAttack.useAbility(target.some)
            val (updatedAttacker2, updatedTarget2) = extraAttack.useAbility(updatedTarget)

            (updatedAttacker2, updatedTarget2)
          } else {
            val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)

            (updatedAttacker, updatedTarget.some)
          }
        case None => (combatant, None)
      }
    }

    def update: Fighter = (_abilityUsages composeLens actionSurgeUsedLens).set(true)(fighter)
  }

}
