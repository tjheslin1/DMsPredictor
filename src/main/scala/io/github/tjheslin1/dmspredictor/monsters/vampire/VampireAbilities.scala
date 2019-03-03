package io.github.tjheslin1.dmspredictor.monsters.vampire

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire.{CharmDC, UnarmedStrike}
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object VampireAbilities extends LazyLogging {

  def bite(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val vampire = combatant.creature.asInstanceOf[Vampire]

    val name: String = "Bite (Vampire)"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = SingleAttack

    def triggerMet(others: List[Combatant]): Boolean =
      others.exists(_.creature.conditions.contains(Grappled(UnarmedStrike.GrappleDc)))

    def conditionMet: Boolean = vampire.biteUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"Vampire used $name")

      val grappledEnemies =
        players(others).filter(_.creature.conditions.contains(Grappled(UnarmedStrike.GrappleDc)))

      nextToFocus(grappledEnemies, focus) match {
        case None => (combatant, others)
        case Some(grappledTarget) =>
          val attackResult = attack(combatant, Bite, grappledTarget)

          val (updatedVampireCombatant, updatedTarget) =
            resolveDamage(combatant, grappledTarget, Bite, attackResult)

          val necroticDamage = attackResult match {
            case CriticalHit  => Bite.necroticDamage + Bite.necroticDamage
            case Hit          => Bite.necroticDamage
            case Miss         => 0
            case CriticalMiss => 0
          }

          val updatedVampire = updatedVampireCombatant.creature.asInstanceOf[Vampire]

          val restoredVampire = Combatant.creatureLens.set(
            updatedVampire.restoreHealth(necroticDamage))(updatedVampireCombatant)

          val updatedHealth    = updatedTarget.creature.health - necroticDamage
          val updatedMaxHealth = updatedTarget.creature.maxHealth - necroticDamage

          val updatedHealthTarget = (Combatant.creatureLens composeLens Creature.creatureHealthLens)
            .set(updatedHealth)(updatedTarget)
          val updatedMaxHealthTarget =
            (Combatant.creatureLens composeLens Creature.creatureMaxHealthLens)
              .set(updatedMaxHealth)(updatedHealthTarget)

          (restoredVampire, others.replace(updatedMaxHealthTarget))
      }
    }

    def update: Creature = vampire.copy(biteUsed = true, firstAttack = false)
  }

  def unarmedStrike(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val vampire = combatant.creature.asInstanceOf[Vampire]

    val name: String = "Unarmed Strike (Vampire)"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = SingleAttack

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean                        = true

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"Vampire used $name")

      val enemies = players(others)

      nextToFocus(enemies, focus) match {
        case None => (combatant, others)
        case Some(target) =>
          val updatedTarget = attack(combatant, UnarmedStrike, target) match {
            case CriticalMiss | Miss =>
              target
            case attackResult @ (CriticalHit | Hit) =>
              if (vampire.firstAttack &&
                  enemies.exists(_.creature.conditions
                    .map(_.name)
                    .contains(Grappled(UnarmedStrike.GrappleDc))) == false) {

                val updatedConditions =
                  target.creature.conditions :+ Grappled(UnarmedStrike.GrappleDc)
                (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
                  .set(updatedConditions)(target)
              } else {
                val (_, updatedAttackTarget) =
                  resolveDamage(combatant, target, UnarmedStrike, attackResult)
                updatedAttackTarget
              }
          }

          (combatant, others.replace(updatedTarget))
      }
    }

    def update: Creature = if (vampire.firstAttack) vampire.copy(firstAttack = false) else vampire
  }

  def charm(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val vampire = combatant.creature.asInstanceOf[Vampire]

    val name: String = "Charm (Vampire)"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = WholeAction

    def triggerMet(others: List[Combatant]): Boolean =
      others.exists(_.creature.conditions.map(_.name).contains(Charmed.name)) == false

    def conditionMet: Boolean = true

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"Vampire used $name")

      nextToFocus(players(others), focus) match {
        case None => (combatant, others)
        case Some(target) =>
          if (savingThrowPassed(CharmDC, Wisdom, target.creature))
            (combatant, List(target))
          else {
            logger.debug(s"${target.creature.name} has been Charmed")

            val charmedTarget = (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
              .set(target.creature.conditions ++ List(Charmed(CharmDC)))(target)

            (combatant, others.replace(charmedTarget))
          }
      }
    }

    def update: Creature = vampire
  }

  case object Bite extends Weapon {
    val name: String           = "Bite (Vampire)"
    val weaponType: WeaponType = Melee
    val damageType: DamageType = Piercing
    val twoHanded: Boolean     = true

    override val hitBonus: Int = 9

    def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6 // +4 from Strength

    def necroticDamage[_: RS] = 3 * D6
  }
}
