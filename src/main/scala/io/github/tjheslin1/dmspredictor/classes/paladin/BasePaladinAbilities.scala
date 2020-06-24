package io.github.tjheslin1.dmspredictor.classes.paladin

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, SingleAttack, WholeAction}
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlot
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellSlots._
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Healing}
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.{monsters, players}
import io.github.tjheslin1.dmspredictor.util.ListOps._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object BasePaladinAbilities extends LazyLogging {

  def layOnHands(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val basePaladin = combatant.creature.asInstanceOf[BasePaladin]

      val name             = "Lay on Hands"
      val order            = currentOrder
      val levelRequirement = LevelOne
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean =
        players(others)
          .filter(_.creature.isAlive)
          .exists(ally => ally.creature.health <= (ally.creature.maxHealth / 2))

      def conditionMet: Boolean = basePaladin.layOnHandsPool > 0

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${basePaladin.name} used $name")

        nextToFocus(combatant, players(others), Healing) match {
          case None => (combatant, others)
          case Some(target) =>
            val healthToRestore =
              Math.min(
                basePaladin.layOnHandsPool,
                target.creature.maxHealth - target.creature.health)

            val updatedPool        = basePaladin.layOnHandsPool - healthToRestore
            val updatedBasePaladin = BasePaladin.layOnHandsPoolLens.set(updatedPool)(basePaladin)

            val updatedPaladin = Combatant.creatureLens.set(updatedBasePaladin)(combatant)

            val updatedTarget = (Combatant.creatureLens composeLens Creature.creatureHealthLens)
              .set(target.creature.health + healthToRestore)(target)

            (updatedPaladin, others.replace(updatedTarget))
        }
      }
      def update: Creature = basePaladin
    }

  def divineSmite(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val basePaladin = combatant.creature.asInstanceOf[BasePaladin]

      val name             = "Divine Smite"
      val order            = currentOrder
      val levelRequirement = LevelTwo
      val abilityAction    = SingleAttack

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean =
        basePaladin.level.value >= 2 &&
          highestSpellSlotAvailable(basePaladin.spellSlots).isDefined

      def damageFromSpellSlot[_: RS](spellSlot: SpellSlot): Int =
        spellSlot.spellLevel.value match {
          case 1 => 2 * D8
          case 2 => 3 * D8
          case 3 => 4 * D8
          case _ => 5 * D8
        }

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${basePaladin.name} used $name")

        val optSpellSlot = highestSpellSlotAvailable(basePaladin.spellSlots)

        val optTarget = nextToFocus(combatant, monsters(others), focus)

        (optSpellSlot, optTarget) match {
          case (None, _) => (combatant, others)
          case (_, None) => (combatant, others)
          case (Some(spellSlot), Some(target)) =>
            val otherCombatants = others.except(target)
            val paladinsWeapon  = combatant.creature.weapon

            attack(combatant, paladinsWeapon, target) match {
              case (Miss | CriticalMiss, updatedTarget) =>
                (combatant, others.replace(updatedTarget))
              case (attackResult, updatedTarget) =>
                val (updatedPaladin, updatedDamagedTarget, updatedOthers) =
                  resolveDamage(
                    combatant,
                    updatedTarget,
                    otherCombatants,
                    paladinsWeapon,
                    attackResult)

                if (updatedDamagedTarget.creature.isConscious) {
                  val smiteDamage =
                    (attackResult, updatedDamagedTarget.creature.creatureType) match {
                      case (CriticalHit, Undead | Fiend) =>
                        damageFromSpellSlot(spellSlot) + damageFromSpellSlot(spellSlot) + (2 * D8)
                      case (Hit, Undead | Fiend) =>
                        damageFromSpellSlot(spellSlot) + (1 * D8)
                      case (CriticalHit, _) =>
                        damageFromSpellSlot(spellSlot) + damageFromSpellSlot(spellSlot)
                      case _ => damageFromSpellSlot(spellSlot)
                    }

                  val smiteDamagedCreature =
                    updatedDamagedTarget.creature.updateHealth(smiteDamage, Radiant, attackResult)

                  val smiteDamagedTarget =
                    Combatant.creatureLens.set(smiteDamagedCreature)(updatedDamagedTarget)

                  val updatedSpellCaster = updatedPaladin.creature.asInstanceOf[SpellCaster]
                  val spellSlotUpdatedCaster = decrementCastersSpellSlot(updatedSpellCaster, spellSlot)

                  val spellSlotUpdatedPaladin = Combatant.creatureLens.set(spellSlotUpdatedCaster)(updatedPaladin)

                  (spellSlotUpdatedPaladin, updatedOthers.replace(smiteDamagedTarget))
                } else {
                  (updatedPaladin, updatedOthers.replace(updatedDamagedTarget))
                }
            }
        }
      }

      def update: Creature = basePaladin
    }

  case class SacredWeaponCondition(turnsLeft: Int = 10) extends PassiveCondition {
    val name = "Sacred Weapon (Condition)"
    val missesTurn = false

    def decrementTurnsLeft(): Condition = SacredWeaponCondition(turnsLeft - 1)
  }

  def sacredWeapon(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val basePaladin = combatant.creature.asInstanceOf[BasePaladin]

      val name             = "Sacred Weapon"
      val order            = currentOrder
      val levelRequirement = LevelThree
      val abilityAction    = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = ???

      def conditionMet: Boolean = ???

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = ???

      def update: Creature = ???
    }
}
