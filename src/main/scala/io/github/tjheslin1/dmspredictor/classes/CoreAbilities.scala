package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.classes.fighter.Fighter
import io.github.tjheslin1.dmspredictor.model.{Combatant, Creature, Level, LevelFive, RS}
import io.github.tjheslin1.dmspredictor.strategy.Ability

object CoreAbilities {

  def extraAttack[T: RS](combatant: Combatant): Ability[T] = new Ability[T](combatant) {
    override def levelRequirement: Level = ???

    override def triggerMet: Boolean = ???

    override def conditionMet: Boolean = ???

    override def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = ???

    override def update: T = ???
  }
}
