package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellAttack
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetAttackSpell extends Spell with LazyLogging {

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def effect[_: RS](spellCaster: SpellCaster,
                    spellLevel: SpellLevel,
                    targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
    val target       = targets.head
    val attackResult = spellAttack(spellCaster, target.creature)

    logger.debug(s"casting $name - $attackResult")

    val dmg = attackResult match {
      case CriticalHit  => damage(spellCaster, spellLevel) + damage(spellCaster, spellLevel)
      case Hit          => damage(spellCaster, spellLevel)
      case Miss         => 0
      case CriticalMiss => 0
    }

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

    (spellCaster, targets.replace(damagedTarget))
  }
}
