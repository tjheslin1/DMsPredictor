package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._

abstract class MultiTargetSavingThrowSpell extends Spell with LazyLogging {

  val isSingleTargetSpell = false
  val isMultiTargetSpell  = true

  val requiresConcentration = false

  val attribute: Attribute
  val halfDamageOnSave: Boolean

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpellEffect

  def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    val damageRoll = damage(spellCaster, spellLevel)

    val updatedTargets = targets.map { target =>
      val (passed, updatedTarget) = spellSavingThrowPassed(spellCaster, attribute, target.creature)

      logger.debug(s"casting $name - Saving throw ${if (passed)
          "Passed"
        else
          "Failed"}")

      val dmg =
        if (passed == false)
          damageRoll
        else if (passed && halfDamageOnSave)
          Math.floor(damageRoll / 2).toInt
        else
          0

      val attackResult =
        if (passed)
          Miss
        else
          Hit

      val updatedHealthTarget = updatedTarget.updateHealth(dmg, damageType, attackResult)
      Combatant.creatureLens.set(updatedHealthTarget)(target)
    }

    (spellCaster, updatedTargets)
  }
}
