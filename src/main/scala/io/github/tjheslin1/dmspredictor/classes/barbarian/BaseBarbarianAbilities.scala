package io.github.tjheslin1.dmspredictor.classes.barbarian

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.ClassAbilities._
import io.github.tjheslin1.dmspredictor.classes.Player.playerBonusActionUsedLens
import io.github.tjheslin1.dmspredictor.classes.barbarian.BaseBarbarian._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamage
import io.github.tjheslin1.dmspredictor.model.Creature.creatureDamageResistancesLens
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.ListOps._

object BaseBarbarianAbilities extends LazyLogging {

  def rage(
      currentOrder: Int,
      rageResistances: List[DamageType] = List(Bludgeoning, Piercing, Slashing)
  )(combatant: Combatant): Ability =
    new Ability(combatant) {
      val barbarian = combatant.creature.asInstanceOf[BaseBarbarian]

      val name                         = "Rage"
      val order                        = currentOrder
      val abilityAction: AbilityAction = WholeAction
      val levelRequirement: Level      = LevelOne

      def triggerMet(others: List[Combatant]): Boolean = barbarian.inRage == false
      def conditionMet: Boolean                        = barbarian.rageUsages > 0

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} used Rage")

        val ragingBarbarianCombatant =
          Combatant.creatureLens.set(updateRagingBarbarian(barbarian))(combatant)

        val enemies = monsters(others)
        val target  = nextToFocus(combatant, enemies, focus)

        target match {
          case None => (ragingBarbarianCombatant, others)
          case Some(targetOfAttack) =>
            nextAbilityToUseInConjunction(
              ragingBarbarianCombatant,
              enemies,
              order,
              AbilityAction.MainAction
            ).fold {
              val (updatedAttacker, updatedTarget, updatedOthers) = attackAndDamage(
                ragingBarbarianCombatant,
                targetOfAttack,
                others)

              (updatedAttacker, updatedOthers.replace(updatedTarget))
            } { nextAbility =>
              val (updatedAttacker, updatedEnemies) = useAdditionalAbility(
                nextAbility,
                ragingBarbarianCombatant,
                enemies,
                focus)
              (updatedAttacker, others.replace(updatedEnemies))
            }
        }
      }

      def update: Creature = barbarian

      private def updateRagingBarbarian(unragedBarbarian: BaseBarbarian): Creature = {
        val updatedRageUsages = unragedBarbarian.rageUsages - 1

        val updatedBarbarian       = rageUsagesLens.set(updatedRageUsages)(unragedBarbarian)
        val rageTurnsLeftBarbarian = rageTurnsLeftLens.set(10)(updatedBarbarian)

        val resistantBarbarian = creatureDamageResistancesLens
          .set(barbarian.damageResistances ++ rageResistances)(rageTurnsLeftBarbarian)
          .asInstanceOf[BaseBarbarian]

        val bonusActionUsedBarbarian = playerBonusActionUsedLens
          .set(true)(resistantBarbarian)
          .asInstanceOf[BaseBarbarian]

        inRageLens.set(true)(bonusActionUsedBarbarian)
      }
    }

  def recklessAttack(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val barbarian = combatant.creature.asInstanceOf[BaseBarbarian]

      val name                         = "Reckless Attack"
      val order                        = currentOrder
      val abilityAction: AbilityAction = SingleAttack
      val levelRequirement: Level      = LevelTwo

      def triggerMet(others: List[Combatant]) = true
      val conditionMet: Boolean               = barbarian.level >= levelRequirement

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${combatant.creature.name} is recklessly attacking")

        val recklessBarbarianCombatant =
          Combatant.creatureLens.set(updateRecklessBarbarian(barbarian))(combatant)

        val target = nextToFocus(combatant, monsters(others), focus)

        target match {
          case None => (recklessBarbarianCombatant, others)
          case Some(targetOfAttack) =>
            val (updatedAttacker, updatedTarget, updatedOthers) = attackAndDamage(
              recklessBarbarianCombatant,
              targetOfAttack,
              others)

            (updatedAttacker, updatedOthers.replace(updatedTarget))
        }
      }

      def update: Creature = barbarian

      private def updateRecklessBarbarian(calmBarbarian: BaseBarbarian): Creature = {
        val advantageBarbarian = Creature.creatureAttackStatusLens.set(Advantage)(calmBarbarian)

        Creature.creatureDefenseStatusLens.set(Disadvantage)(advantageBarbarian)
      }
    }
}
