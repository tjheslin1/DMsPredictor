package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.SavingThrow.savingThrowPassed
import io.github.tjheslin1.dmspredictor.model._

object Concentration extends LazyLogging {

  def handleConcentration[_: RS](spellCaster: SpellCaster, damageTaken: Int): SpellCaster = {
    val dc = concentrationDifficultyClass(damageTaken)

    if (savingThrowPassed(dc, Constitution, spellCaster)) spellCaster
    else {
      logger.debug(s"${spellCaster.name} loses concentration")

      SpellCaster.concentratingLens.set(None)(spellCaster)
    }
  }

  def concentrationDifficultyClass(damageTaken: Int): Int = {
    val halfDamageTaken = Math.floor(damageTaken / 2).toInt

    if (halfDamageTaken > 10) halfDamageTaken else 10
  }
}
