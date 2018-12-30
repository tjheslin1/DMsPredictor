package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.model.{Combatant, Creature, Level, LevelFive, RS}
import io.github.tjheslin1.dmspredictor.strategy.Ability

object CoreAbilities {

  def extraAttack[T: RS](combatant: Combatant) = new Ability[T](combatant) {
    val levelRequirement = LevelFive

    def triggerMet: Boolean = ???

    def conditionMet: Boolean = ???

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = ???

    def update: T = ???
  }
}
