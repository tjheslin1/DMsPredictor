package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._

object Concentration extends LazyLogging {

  def handleConcentration[_: RS](
      spellCaster: SpellCaster,
      buffSpell: Spell,
      damageTaken: Int
  ): SpellCaster = {
    val dc = concentrationDifficultyClass(damageTaken)

    val (passed, updatedSpellCaster: SpellCaster) = savingThrowPassed(dc, Constitution, spellCaster)

    if (passed) updatedSpellCaster
    else {
      logger.debug(s"${updatedSpellCaster.name} loses concentration")

      buffSpell match {
        case selfBuffSpell: SelfBuffSpell =>
          val selfBuffHandledCaster =
            selfBuffSpell.onLossOfConcentrationLoseBuff(updatedSpellCaster)

          SpellCaster.concentratingLens.set(None)(selfBuffHandledCaster)
        case multiTargetBuffSpell: MultiTargetBuffSpell =>
          val buffHandledCaster =
            multiTargetBuffSpell.onLossOfConcentrationLoseBuff(updatedSpellCaster)

          SpellCaster.concentratingLens.set(None)(buffHandledCaster)
        case _ => SpellCaster.concentratingLens.set(None)(updatedSpellCaster)
      }
    }
  }

  def concentrationDifficultyClass(damageTaken: Int): Int = {
    val halfDamageTaken = Math.floor(damageTaken / 2).toInt

    if (halfDamageTaken > 10) halfDamageTaken else 10
  }
}
