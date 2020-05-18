package io.github.tjheslin1.dmspredictor.classes.paladin

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, WholeAction}
import io.github.tjheslin1.dmspredictor.strategy.Focus

object BasePaladinAbilities extends LazyLogging {

  def layOnHands(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val basePaladin = combatant.creature.asInstanceOf[BasePaladin]

      val name = "Lay on Hands"
      val order = currentOrder
      val levelRequirement = LevelOne
      val abilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = ???

      def conditionMet: Boolean = ???

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = ???

      def update: Creature = ???
    }
}
