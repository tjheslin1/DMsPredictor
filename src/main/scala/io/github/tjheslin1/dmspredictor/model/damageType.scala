package io.github.tjheslin1.dmspredictor.model

sealed trait DamageType extends Product with Serializable

object DamageType {
  val allDamageTypes = List(
    Bludgeoning,
    Piercing,
    Slashing,
    Magical,
    Acid,
    Cold,
    Fire,
    Lightning,
    Necrotic,
    Poison,
    Radiant,
    Thunder,
    Psychic
  )
}

case object Bludgeoning extends DamageType
case object Piercing    extends DamageType
case object Slashing    extends DamageType
case object Magical     extends DamageType

case object Acid      extends DamageType
case object Cold      extends DamageType
case object Fire      extends DamageType
case object Lightning extends DamageType
case object Necrotic extends DamageType
case object Poison    extends DamageType
case object Radiant   extends DamageType
case object Thunder   extends DamageType
case object Psychic   extends DamageType
