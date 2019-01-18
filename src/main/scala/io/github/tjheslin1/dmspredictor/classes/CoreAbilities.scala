package io.github.tjheslin1.dmspredictor.classes

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Actions.attackAndDamageTimes
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.Ability

object CoreAbilities {

  def extraAttack(combatant: Combatant): Ability = new Ability(combatant) {
    val levelRequirement: Level = LevelFive

    def triggerMet: Boolean   = true
    def conditionMet: Boolean = true

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) =
      target match {
        case None => (combatant, None)
        case Some(target: Combatant) =>
          val (updatedAttacker, updatedTarget) = attackAndDamageTimes(2, combatant, target)
          (updatedAttacker, updatedTarget.some)
      }

    def update: Creature = combatant.creature
  }
}
