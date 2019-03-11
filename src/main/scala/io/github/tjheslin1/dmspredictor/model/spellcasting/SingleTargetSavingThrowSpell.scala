package io.github.tjheslin1.dmspredictor.model.spellcasting

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell._
import io.github.tjheslin1.dmspredictor.util.ListOps._

abstract class SingleTargetSavingThrowSpell extends Spell with LazyLogging {

  val attribute: Attribute
  val halfDamageOnSave: Boolean

  val damageType: DamageType
  val spellEffect: SpellEffect = DamageSpell

  def damage[_: RS](spellCaster: SpellCaster): Int

  def effect[_: RS](spellCaster: SpellCaster,
                    targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
    val target            = targets.head
    val savingThrowPassed = spellSavingThrowPassed(spellCaster, attribute, target.creature)

    logger.debug(s"casting $name - Saving throw ${if (savingThrowPassed) "Passed" else "Failed"}")

    val dmg =
      if (savingThrowPassed == false) damage(spellCaster)
      else if (savingThrowPassed && halfDamageOnSave) Math.floor(damage(spellCaster) / 2).toInt
      else 0

    val attackResult = if (savingThrowPassed) Miss else Hit

    val damagedTarget =
      target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

    (spellCaster, targets.replace(damagedTarget))
  }
}
