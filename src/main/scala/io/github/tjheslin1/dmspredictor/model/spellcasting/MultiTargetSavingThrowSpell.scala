package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._

abstract class MultiTargetSavingThrowSpell extends Spell with LazyLogging {

  val attribute: Attribute
  val halfDamageOnSave: Boolean

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    val damageRoll = damage(spellCaster, spellLevel)

    val updatedTargets = targets.map { target =>
      val savingThrowPassed = spellSavingThrowPassed(spellCaster, attribute, target.creature)

      logger.debug(s"casting $name - Saving throw ${if (savingThrowPassed) "Passed" else "Failed"}")

      val dmg =
        if (savingThrowPassed == false) damageRoll
        else if (savingThrowPassed && halfDamageOnSave)
          Math.floor(damageRoll / 2).toInt
        else 0

      val attackResult = if (savingThrowPassed) Miss else Hit
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))
    }

    (spellCaster, updatedTargets)
  }
}
