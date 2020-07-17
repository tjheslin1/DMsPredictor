package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class MultiTargetBuffSpell extends Spell with LazyLogging {

  val isSingleTargetSpell = false
  val isMultiTargetSpell  = true

  val buffCondition: Condition
  val affectedTargets: Int

  val buffTargetsPriority: Ordering[Combatant]

  val spellEffect: SpellEffect = BuffSpellEffect

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    logger.debug(s"${spellCaster.name} cast $name")

    val buffTargets = targets.sorted(buffTargetsPriority).take(affectedTargets)

    val updatedBuffTargets = buffTargets.map { target =>
      val currentConditions = target.creature.conditions

      (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
        .set(currentConditions :+ buffCondition)(target)
    }

    val buffedSpellCaster = if (buffTargets.size < affectedTargets) {
      val currentConditions = spellCaster.conditions

      Creature.creatureConditionsLens
        .set(currentConditions :+ buffCondition)(spellCaster)
        .asInstanceOf[SpellCaster]
    } else spellCaster

    val updatedSpellCaster =
      if (requiresConcentration)
        SpellCaster.concentratingLens.set(this.some)(buffedSpellCaster)
      else
        buffedSpellCaster

    (updatedSpellCaster, targets.replace(updatedBuffTargets))
  }

  def onLossOfConcentrationLoseBuff(spellCaster: SpellCaster): SpellCaster = {
    val updatedConditions = spellCaster.conditions diff List(buffCondition)

    Creature.creatureConditionsLens.set(updatedConditions)(spellCaster).asInstanceOf[SpellCaster]
  }
}
