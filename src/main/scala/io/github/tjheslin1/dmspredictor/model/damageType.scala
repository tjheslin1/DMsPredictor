package io.github.tjheslin1.dmspredictor.model

sealed trait DamageType extends Product with Serializable

case object Bludgeoning extends DamageType
case object Piercing    extends DamageType
case object Slashing    extends DamageType
case object Magical     extends DamageType

case object Acid      extends DamageType
case object Cold      extends DamageType
case object Fire      extends DamageType
case object Lightning extends DamageType
case object Poison    extends DamageType
case object Thunder   extends DamageType
