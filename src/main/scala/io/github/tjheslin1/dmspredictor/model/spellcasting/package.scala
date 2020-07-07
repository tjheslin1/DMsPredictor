package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

package object spellcasting {

  type SpellLevel = Int Refined Interval.ClosedOpen[W.`0`.T, W.`10`.T]

  sealed trait SchoolOfMagic extends Product with Serializable

  case object Evocation     extends SchoolOfMagic
  case object Abjuration    extends SchoolOfMagic
  case object Enchantment   extends SchoolOfMagic
  case object Conjuration   extends SchoolOfMagic
  case object Divination    extends SchoolOfMagic
  case object Necromancy    extends SchoolOfMagic
  case object Transmutation extends SchoolOfMagic

  sealed trait CastingTime extends Product with Serializable

  case object OneActionCast   extends CastingTime
  case object BonusActionCast extends CastingTime

  sealed trait SpellTargetStyle extends Product with Serializable

  case object MeleeSpellAttack                      extends SpellTargetStyle
  case object RangedSpellAttack                     extends SpellTargetStyle
  case class SpellSavingThrow(attribute: Attribute) extends SpellTargetStyle

  sealed trait SpellEffect extends Product with Serializable

  case object DamageSpellEffect  extends SpellEffect
  case object HealingSpellEffect extends SpellEffect

  case object ConditionSpellEffect extends SpellEffect
  case object BuffSpellEffect          extends SpellEffect
  case object InstantEffectSpellEffect extends SpellEffect
}
