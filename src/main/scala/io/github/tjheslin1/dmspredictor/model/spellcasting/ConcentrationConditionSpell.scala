package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class ConcentrationConditionSpell extends Spell with LazyLogging {

  val singleTarget: Boolean
  val attribute: Attribute
  val spellEffect: SpellEffect       = ConcentrationSpell
  val requiresConcentration: Boolean = true

  def conditionFrom(spellCaster: SpellCaster): Condition

  def applyCondition[_: RS](spellCaster: SpellCaster, target: Combatant): Combatant = {
    val condition = conditionFrom(spellCaster)

    logger.debug(s"${target.creature.name} is now affected by ${condition.name}")

    val currentConditions = target.creature.conditions
    (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
      .set(currentConditions ++ List(condition))(target)
  }

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      allTargets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {

    val targets = if (singleTarget) List(allTargets.head) else allTargets

    logger.debug(s"${spellCaster.name} is casting $name")

    val updatedTargets = targets.map { target =>
      val savingThrowPassed = spellSavingThrowPassed(spellCaster, attribute, target.creature)

      val savingThrowResult = if (savingThrowPassed) "Passed" else "Failed"
      logger.debug(s"${target.creature.name} rolls saving throw - $savingThrowResult")

      if (savingThrowPassed) target
      else applyCondition(spellCaster, target)
    }

    def anyTargetIsAffectedByCondition(): Boolean =
      updatedTargets.exists(_.creature.conditions.contains(conditionFrom(spellCaster)))

    if (requiresConcentration && anyTargetIsAffectedByCondition()) {
      val updatedConcentratingSpellCaster =
        SpellCaster.concentratingLens.set(this.some)(spellCaster)

      (updatedConcentratingSpellCaster, allTargets.replace(updatedTargets))
    } else
      (spellCaster, allTargets.replace(updatedTargets))
  }
}
