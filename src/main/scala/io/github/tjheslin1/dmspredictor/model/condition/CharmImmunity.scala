package io.github.tjheslin1.dmspredictor.model.condition

case object CharmImmunity extends PassiveCondition {

  val turnsLeft  = Integer.MAX_VALUE
  val name       = "Charm Immunity"
  val missesTurn = false

  def decrementTurnsLeft(): Condition = this
}
