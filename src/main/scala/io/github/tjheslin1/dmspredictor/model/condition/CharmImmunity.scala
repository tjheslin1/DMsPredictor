package io.github.tjheslin1.dmspredictor.model.condition
import io.github.tjheslin1.dmspredictor.model.{Creature, RS}

case object CharmImmunity extends PassiveCondition {

  val name       = "Charm Immunity"
  val missesTurn = false

  def decrementTurnsLeft(): Condition = this
}
