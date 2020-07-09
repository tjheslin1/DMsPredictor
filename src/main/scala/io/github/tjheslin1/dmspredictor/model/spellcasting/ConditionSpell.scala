package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class ConditionSpell extends Spell with LazyLogging {

  val spellEffect = ConditionSpellEffect

  val affectedTargets: Int
  val attribute: Attribute

  val conditionTargetsPriority: Ordering[Combatant]

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
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    logger.debug(s"${spellCaster.name} is casting $name")

    val conditionTargets = targets.sorted(conditionTargetsPriority).take(affectedTargets)

    val updatedTargets = conditionTargets.map { target =>
      val (passed, updatedCreature) =
        spellSavingThrowPassed(spellCaster, attribute, target.creature)

      val updatedTarget = Combatant.creatureLens.set(updatedCreature)(target)

      val savingThrowResult = if (passed) "Passed" else "Failed"
      logger.debug(s"${updatedCreature.name} rolls saving throw - $savingThrowResult")

      if (passed) updatedTarget
      else applyCondition(spellCaster, updatedTarget)
    }

    def anyTargetIsAffectedByCondition(): Boolean =
      updatedTargets.exists(_.creature.conditions.contains(conditionFrom(spellCaster)))

    if (requiresConcentration && anyTargetIsAffectedByCondition()) {
      val updatedConcentratingSpellCaster =
        SpellCaster.concentratingLens.set(this.some)(spellCaster)

      (updatedConcentratingSpellCaster, targets.replace(updatedTargets))
    } else
      (spellCaster, targets.replace(updatedTargets))
  }
}
