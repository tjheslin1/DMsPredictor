package io.github.tjheslin1.dmspredictor.monsters.vampire

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.model.condition.Condition.addCondition
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire.{CharmDC, UnarmedStrike}
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object VampireAbilities extends LazyLogging {

  def bite(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val vampire = combatant.creature.asInstanceOf[Vampire]

      val name  = "Bite (Vampire)"
      val order = currentOrder

      val levelRequirement: Level      = LevelOne
      val abilityAction: AbilityAction = SingleAttack

      def triggerMet(others: List[Combatant]): Boolean =
        others.exists(_.creature.conditions.contains(Grappled(UnarmedStrike.GrappleDc)))

      def conditionMet: Boolean = vampire.biteUsed == false

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"Vampire used $name")

        val grappledEnemies = players(others).filter(
          _.creature.conditions.contains(Grappled(UnarmedStrike.GrappleDc)))

        nextToFocus(combatant, grappledEnemies, focus) match {
          case None =>
            (combatant, others)
          case Some(grappledTarget) =>
            val (attackResult, hitTarget) = attack(combatant, Bite, grappledTarget)

            val (updatedVampireCombatant, updatedTarget, updatedOthers) = resolveDamage(
              combatant,
              hitTarget,
              others,
              Bite,
              attackResult)

            val necroticDamage =
              attackResult match {
                case CriticalHit =>
                  Bite.necroticDamage + Bite.necroticDamage
                case Hit =>
                  Bite.necroticDamage
                case Miss =>
                  0
                case CriticalMiss =>
                  0
              }

            if (necroticDamage > 0)
              logger.debug(s"$name deals $necroticDamage necrotic damage")

            val updatedVampire = updatedVampireCombatant.creature.asInstanceOf[Vampire]

            val restoredVampire =
              Combatant.creatureLens.set(
                updatedVampire.restoreHealth(necroticDamage)
              )(updatedVampireCombatant)

            val updatedHealthTarget =
              Combatant.creatureLens
                .set(updatedTarget.creature.updateHealth(necroticDamage, Necrotic, attackResult))(
                  updatedTarget
                )

            val updatedMaxHealth = updatedHealthTarget.creature.maxHealth - necroticDamage
            val updatedMaxHealthTarget =
              (Combatant.creatureLens composeLens Creature.creatureMaxHealthLens)
                .set(updatedMaxHealth)(updatedHealthTarget)

            (restoredVampire, updatedOthers.replace(updatedMaxHealthTarget))
        }
      }

      def update: Creature = vampire.copy(biteUsed = true, firstAttack = false)
    }

  def unarmedStrike(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val vampire = combatant.creature.asInstanceOf[Vampire]

      val name  = "Unarmed Strike (Vampire)"
      val order = currentOrder

      val levelRequirement: Level      = LevelOne
      val abilityAction: AbilityAction = SingleAttack

      def triggerMet(others: List[Combatant]): Boolean = true
      def conditionMet: Boolean                        = true

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"Vampire used $name")

        val enemies = players(others)

        nextToFocus(combatant, enemies, focus) match {
          case None =>
            (combatant, others)
          case Some(target) =>
            val (attackResult, hitTarget) = attack(combatant, UnarmedStrike, target)
            attackResult match {
              case CriticalMiss | Miss =>
                (combatant, others)
              case attackResult @ (CriticalHit | Hit) =>
                if (
                  vampire.firstAttack &&
                  enemies.exists(
                    _.creature.conditions
                      .contains(Grappled(UnarmedStrike.GrappleDc))
                  ) == false
                ) {

                  logger.debug(s"${vampire.name} grapples ${hitTarget.creature.name}")

                  val updatedConditions =
                    hitTarget.creature.conditions :+ Grappled(UnarmedStrike.GrappleDc)

                  val updatedTarget =
                    (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
                      .set(updatedConditions)(hitTarget)

                  (combatant, others.replace(updatedTarget))
                } else {
                  val (updatedVampire, updatedAttackTarget, updatedOthers) = resolveDamage(
                    combatant,
                    hitTarget,
                    others,
                    UnarmedStrike,
                    attackResult)

                  (updatedVampire, updatedOthers.replace(updatedAttackTarget))
                }
            }
        }
      }

      def update: Creature =
        if (vampire.firstAttack)
          vampire.copy(firstAttack = false)
        else
          vampire
    }

  def charm(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val vampire = combatant.creature.asInstanceOf[Vampire]

      val name  = "Charm (Vampire)"
      val order = currentOrder

      val levelRequirement: Level      = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def charmTargets(others: List[Combatant]): List[Combatant] =
        others
          .filter(_.creature.conditionImmunities.contains(CharmedCondition) == false)
          .filter(_.creature.conditions.map(_.name).contains(CharmImmunity.name) == false)
          .filter(_.creature.conditions.map(_.name).contains(Charmed.name) == false)

      def triggerMet(others: List[Combatant]): Boolean = {
        val nonImmuneCombatants = others
          .filter(_.creature.conditionImmunities.contains(CharmedCondition) == false)
          .filter(_.creature.conditions.map(_.name).contains(CharmImmunity.name) == false)

        if (nonImmuneCombatants.isEmpty)
          false
        else
          nonImmuneCombatants
            .exists(_.creature.conditions.map(_.name).contains(Charmed.name)) == false
      }

      def conditionMet: Boolean = true

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"Vampire used $name")

        nextToFocus(combatant, charmTargets(players(others)), focus) match {
          case None =>
            (combatant, others)
          case Some(target) =>
            val (passed, updatedCreature) = savingThrowPassed(CharmDC, Wisdom, target.creature)

            val updatedTarget = Combatant.creatureLens.set(updatedCreature)(target)

            if (passed)
              (combatant, others.replace(updatedTarget))
            else {
              logger.debug(s"${updatedTarget.creature.name} has been Charmed")

              val charmedTarget = addCondition(updatedTarget, Charmed(CharmDC))

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
    val finesse                = false

    override val hitBonus: Int = 9

    def damage(implicit rollStrategy: RollStrategy): Int = 1 * D6 // +4 from Strength

    def necroticDamage[_: RS] = 3 * D6
  }
}
