package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model._

abstract class MultiTargetBuffSpell extends Spell with LazyLogging {

  val buffCondition: Condition
  val affectedTargets: Int

  val buffCaster: Boolean

  val spellEffect: SpellEffect = BuffSpell

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    logger.debug(s"${spellCaster.name} cast $name")

    (spellCaster, targets)
  }

  def onLossOfConcentration(spellCaster: SpellCaster): SpellCaster = ???
}
