package io.github.tjheslin1.dmspredictor.model

trait AttackStatus extends Product with Serializable

case object Advantage    extends AttackStatus
case object Disadvantage extends AttackStatus
case object Regular      extends AttackStatus
