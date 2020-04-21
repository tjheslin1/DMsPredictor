package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellAttack
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetAttackSpell extends Spell with LazyLogging {

  val halfDamageOnMiss: Boolean

  val damageType: DamageType
  val spellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int

  def additionalEffect(target: Combatant, attackResult: AttackResult): Combatant = target

  def effect[_: RS](
      spellCaster: SpellCaster,
      spellLevel: SpellLevel,
      targets: List[Combatant]
  ): (SpellCaster, List[Combatant]) = {
    val target       = targets.head
    val attackResult = spellAttack(spellCaster, target.creature)

    logger.debug(s"${spellCaster.name} is casting $name - $attackResult")

    val dmg = attackResult match {
      case CriticalHit => damage(spellCaster, spellLevel) + damage(spellCaster, spellLevel)
      case Hit         => damage(spellCaster, spellLevel)
      case Miss =>
        if (halfDamageOnMiss) Math.floor(damage(spellCaster, spellLevel) / 2).toInt else 0
      case CriticalMiss => 0
    }

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

    val additionalEffectedTarget = additionalEffect(damagedTarget, attackResult)

    (spellCaster, targets.replace(additionalEffectedTarget))
  }
}
