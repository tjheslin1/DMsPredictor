package io.github.tjheslin1.dmspredictor.model.spellcasting

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf, Zombie}
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class MultiTargetBuffSpell extends Spell with LazyLogging {

  val buffCondition: Condition
  val affectedTargets: Int

  val buffTargetsPriority: Ordering[Combatant]

  val spellEffect: SpellEffect = BuffSpell

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

  def onLossOfConcentration(spellCaster: SpellCaster): SpellCaster = {
    val updatedConditions = spellCaster.conditions diff List(buffCondition)

    Creature.creatureConditionsLens.set(updatedConditions)(spellCaster).asInstanceOf[SpellCaster]
  }
}

object MultiTargetBuffSpell {

  def focusTanksCreatureOrder(creature: Creature): Int =
    creature match {
      case _: Barbarian => 1
      case _: Berserker => 1
      case _: Fighter   => 2
      case _: Champion  => 2
      case _: Rogue     => 3
      case _: Ranger    => 4
      case _: Hunter    => 4
      case _: Paladin   => 5
      case _: Wizard    => 6
      case _: Cleric    => 7

      case _: Lich     => 8
      case _: Vampire  => 9
      case _: Werewolf => 10
      case _: Goblin   => 11
      case _: Zombie   => 12
    }
}
