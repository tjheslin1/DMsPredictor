package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object WizardSpells {

  case object MagicMissile extends SingleTargetAttackSpell {
    val name                   = "Magic Missile"
    val damageType: DamageType = Force

    val school: SchoolOfMagic              = Evocation
    val castingTime: CastingTime           = OneAction
    val spellTargetStyle: SpellTargetStyle = RangedSpellAttack
    val spellLevel: SpellLevel             = 1
    val concentration: Boolean             = false

    def damage[_: RS](spellCaster: SpellCaster): Int = (3 * D4) + 3

    /*
    Magic Missile always Hits
     */
    override def effect[_: RS](spellCaster: SpellCaster,
                               targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
      val target = targets.head
      logger.debug(s"casting $name")

      val damagedTarget =
        target.copy(creature = target.creature.updateHealth(damage(spellCaster), damageType, Hit))

      (spellCaster, targets.replace(damagedTarget))
    }
  }
}
