package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition

abstract class SelfBuffSpell extends Spell with LazyLogging {

  val selfBuffCondition: Condition

  val spellEffect: SpellEffect = BuffSpell

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {

    val currentConditions = spellCaster.conditions
    val conditionAppliedCaster = Creature.creatureConditionsLens
      .set(currentConditions :+ selfBuffCondition)(spellCaster)
      .asInstanceOf[SpellCaster]

    val buffedSpellCaster =
      if (requiresConcentration)
        SpellCaster.concentratingLens.set(this.some)(conditionAppliedCaster)
      else
        conditionAppliedCaster

    (buffedSpellCaster, targets)
  }

  def onLossOfConcentration(spellCaster: SpellCaster): SpellCaster = {
    val updatedConditions = spellCaster.conditions diff List(selfBuffCondition)

    Creature.creatureConditionsLens.set(updatedConditions)(spellCaster).asInstanceOf[SpellCaster]
  }
}
