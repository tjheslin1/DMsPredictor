package io.github.tjheslin1.dmspredictor.model.condition

case object VampireCharmImmunity extends PassiveCondition {

  val name       = "Vampire Charm Immunity"
  val missesTurn = false

  def decrementTurnsLeft(): Condition = this
}
