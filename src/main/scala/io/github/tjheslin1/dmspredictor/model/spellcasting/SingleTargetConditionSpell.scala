package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSavingThrowPassed
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetConditionSpell extends Spell with LazyLogging {

  val attribute: Attribute
  val spellEffect: SpellEffect = ConditionSpell

  def conditionFrom(spellCaster: SpellCaster): Condition

  def applyCondition[_: RS](spellCaster: SpellCaster, target: Combatant): Combatant = {
    val condition = conditionFrom(spellCaster)

    logger.debug(s"${target.creature.name} is now ${condition.name}")

    val currentConditions = target.creature.conditions
    (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
      .set(currentConditions ++ List(condition))(target)
  }

  def effect[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel,
                    targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
    val target = targets.head

    val savingThrowPassed = spellSavingThrowPassed(spellCaster, attribute, target.creature)

    logger.debug(s"casting $name - Saving throw ${if (savingThrowPassed) "Passed" else "Failed"}")

    if (savingThrowPassed) (spellCaster, targets)
    else {
      val updatedTarget = applyCondition(spellCaster, target)

      (spellCaster, targets.replace(updatedTarget))
    }
  }
}
