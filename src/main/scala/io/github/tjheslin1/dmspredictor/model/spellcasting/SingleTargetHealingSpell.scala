package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetHealingSpell extends Spell with LazyLogging {

  val requiresConcentration = false

  val spellEffect: SpellEffect = HealingSpell

  def healing[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    val target = targets.head

    val healthRestored = healing(spellCaster, spellLevel)
    val updatedHealth  = Math.min(target.creature.maxHealth, target.creature.health + healthRestored)

    logger.debug(s"${target.creature.name} was healed for $healthRestored")

    val updatedTarget =
      (Combatant.creatureLens composeLens Creature.creatureHealthLens).set(updatedHealth)(target)

    (spellCaster, targets.replace(updatedTarget))
  }
}
