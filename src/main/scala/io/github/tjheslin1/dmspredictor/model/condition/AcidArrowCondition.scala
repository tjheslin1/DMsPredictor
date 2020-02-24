package io.github.tjheslin1.dmspredictor.model.condition

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.SpellLevel
import monocle.macros.Lenses
import io.github.tjheslin1.dmspredictor.util.IntOps._

@Lenses("_") case class AcidArrowCondition(spellLevel: SpellLevel, turnsLeft: Int = 1) extends EndOfTurnCondition {
  val name: String            = "Acid Arrow (condition)"
  val saveDc: Int             = 0
  val missesTurn: Boolean     = false
  val handleOnDamage: Boolean = false

  def decrementTurnsLeft(): Condition = AcidArrowCondition(spellLevel, turnsLeft = 0)

  def handleEndOfTurn[_: RS](creature: Creature): Creature =
    creature.updateHealth(spellLevel.value * D4, Acid, Hit)
}
