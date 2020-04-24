package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetSavingThrowSpell extends Spell with LazyLogging {

  val savingThrowAttribute: Attribute
  val halfDamageOnSave: Boolean

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def additionalEffect[_: RS](
      spellCaster: SpellCaster,
      target: Combatant,
      others: List[Combatant],
      savingThrowPassed: Boolean
  ): (SpellCaster, Combatant, List[Combatant]) = (spellCaster, target, others)

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    val (List(target), others) = targets.splitAt(1)
    val savingThrowPassed =
      spellSavingThrowPassed(spellCaster, savingThrowAttribute, target.creature)

    logger.debug(
      s"${spellCaster.name} is casting $name  on ${target.creature.name} " +
        s"- Saving throw ${if (savingThrowPassed) "Passed" else "Failed"}"
    )

    val dmg =
      if (savingThrowPassed == false) damage(spellCaster, spellLevel)
      else if (savingThrowPassed && halfDamageOnSave)
        Math.floor(damage(spellCaster, spellLevel) / 2).toInt
      else 0

    val attackResult = if (savingThrowPassed) Miss else Hit

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

    val (updatedSpellCaster, additionalEffectedTarget, updatedOthers) =
      additionalEffect(spellCaster, damagedTarget, others, savingThrowPassed)

    (updatedSpellCaster, updatedOthers.replace(additionalEffectedTarget))
  }
}
