package io.github.tjheslin1.dmspredictor.model.condition

case object CharmImmunity extends PassiveCondition {

  val name       = "Charm Immunity"
  val missesTurn = false

  def decrementTurnsLeft(): Condition = this
}
