package io.github.tjheslin1.dmspredictor.model

sealed trait DamageType

case object Bludgeoning extends DamageType
case object Piercing    extends DamageType
case object Slashing    extends DamageType
case object Magical     extends DamageType
