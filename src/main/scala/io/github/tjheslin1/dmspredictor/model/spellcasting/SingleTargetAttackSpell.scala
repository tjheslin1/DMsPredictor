package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellAttackBonus
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetAttackSpell extends Spell with LazyLogging {

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster): Int

  def effect[_: RS](spellCaster: SpellCaster,
                    targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
    val target       = targets.head
    val attackResult = spellAttack(spellCaster, target.creature)

    logger.debug(s"casting $name - $attackResult")

    val dmg = attackResult match {
      case CriticalHit  => damage(spellCaster) + damage(spellCaster)
      case Hit          => damage(spellCaster)
      case Miss         => 0
      case CriticalMiss => 0
    }

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

    (spellCaster, targets.replace(damagedTarget))
  }

  private def spellAttack[_: RS](spellCaster: SpellCaster, target: Creature): AttackResult =
    D20.roll() match {
      case roll if spellCaster.scoresCritical(roll) => CriticalHit
      case 1                                        => CriticalMiss
      case roll =>
        if ((roll + spellAttackBonus(spellCaster)) >= target.armourClass) Hit else Miss
    }
}
