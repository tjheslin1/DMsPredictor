package io.github.tjheslin1.dmspredictor.classes.paladin

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, SingleAttack, WholeAction}
import io.github.tjheslin1.dmspredictor.strategy.{Focus, Healing}
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.ListOps._

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

      def triggerMet(others: List[Combatant]): Boolean = ???

      def conditionMet: Boolean = ???

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) =
        ???

      def update: Creature = ???
    }
}
