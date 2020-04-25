package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetInstantEffectSpell extends Spell with LazyLogging {

  val spellEffect = InstantEffectSpell

  def instantEffect(spellCaster: SpellCaster, target: Combatant): (SpellCaster, Combatant)

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {

    logger.debug(s"${spellCaster.name} is casting $name")

    val target = targets.head

    val (updatesSpellCaster, instantEffectAppliedTarget) = instantEffect(spellCaster, target)

    (updatesSpellCaster, targets.replace(instantEffectAppliedTarget))
  }
}