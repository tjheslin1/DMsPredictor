package io.github.tjheslin1.dmspredictor.model

import eu.timepit.refined.W
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval

package object spellcasting {

  type SpellLevel = Int Refined Interval.ClosedOpen[W.`0`.T, W.`10`.T]

  sealed trait SchoolOfMagic extends Product with Serializable

  case object Evocation   extends SchoolOfMagic
  case object Abjuration  extends SchoolOfMagic
  case object Enchantment extends SchoolOfMagic

  sealed trait CastingTime extends Product with Serializable

  case object OneAction   extends CastingTime
  case object BonusAction extends CastingTime

  sealed trait SpellTargetStyle extends Product with Serializable

  case object MeleeSpellAttack                      extends SpellTargetStyle
  case object RangedSpellAttack                     extends SpellTargetStyle
  case class SpellSavingThrow(attribute: Attribute) extends SpellTargetStyle

  sealed trait SpellEffect extends Product with Serializable

  case object DamageSpell    extends SpellEffect
  case object HealingSpell   extends SpellEffect
  case object ConditionSpell extends SpellEffect
}
