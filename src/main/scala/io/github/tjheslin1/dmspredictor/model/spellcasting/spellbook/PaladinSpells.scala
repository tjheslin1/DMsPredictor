package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition

object PaladinSpells {

  case object BlessCondition extends Condition {
    val name = "Bless (Condition)"
    val saveDc: Int = ???
    val turnsLeft: Int = ???
    val missesTurn: Boolean = ???
    val isHandledOnDamage: Boolean = ???

    def decrementTurnsLeft(): Condition = ???

    def handleStartOfTurn[_: RS](creature: Creature): Creature = ???

    def handleEndOfTurn[_: RS](creature: Creature): Creature = ???

    def handleOnDamage[_: RS](creature: Creature, damage: Int): Creature = ???

    def onConditionApplied(creature: Creature): Creature = ???

    def onConditionRemoved(creature: Creature): Creature = ???
  }
}
