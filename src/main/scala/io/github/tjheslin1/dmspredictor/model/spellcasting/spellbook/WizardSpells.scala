package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{AcidArrowCondition, StartOfTurnCondition}
import io.github.tjheslin1.dmspredictor.model.reaction.OnHitReaction
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellAttack
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object WizardSpells extends LazyLogging {

  case object FireBolt extends SingleTargetAttackSpell {
    val name                   = "Fire Bolt"
    val damageType: DamageType = Fire

    val school                 = Evocation
    val castingTime            = OneActionCast
    val spellTargetStyle       = RangedSpellAttack
    val spellLevel: SpellLevel = 0
    val requiresConcentration  = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = spellCaster match {
      case p: Player if p.level == LevelFive => 2 * D10
      case _                                 => 1 * D10
    }
  }

  case object MagicMissile extends SingleTargetAttackSpell {
    val name                   = "Magic Missile"
    val damageType: DamageType = Force

    val school                 = Evocation
    val castingTime            = OneActionCast
    val spellTargetStyle       = RangedSpellAttack
    val spellLevel: SpellLevel = 1
    val requiresConcentration  = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
      val numberOfDarts = 2 + spellLevel

      (numberOfDarts * D4) + numberOfDarts
    }

    /*
    Magic Missile always Hits
     */
    override def effect[_: RS](spellCaster: SpellCaster,
                               spellLevel: SpellLevel,
                               targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
      val target = targets.head
      logger.debug(s"casting $name")

      val damagedTarget =
        target.copy(
          creature = target.creature.updateHealth(damage(spellCaster, spellLevel), damageType, Hit))

      (spellCaster, targets.replace(damagedTarget))
    }
  }

  case object AcidArrow extends SingleTargetAttackSpell {
    val name: String = "Acid Arrow"
    val damageType   = Acid

    val school                 = Evocation
    val castingTime            = OneActionCast
    val spellLevel: SpellLevel = 2
    val requiresConcentration  = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = 4 * D4

    override def effect[_: RS](spellCaster: SpellCaster,
                               spellLevel: SpellLevel,
                               targets: List[Combatant]): (SpellCaster, List[Combatant]) = {
      val target       = targets.head
      val attackResult = spellAttack(spellCaster, target.creature)

      logger.debug(s"casting $name - $attackResult")

      val dmg = attackResult match {
        case CriticalHit  => damage(spellCaster, spellLevel) + damage(spellCaster, spellLevel)
        case Hit          => damage(spellCaster, spellLevel)
        case Miss         => Math.floor(damage(spellCaster, spellLevel) / 2).toInt
        case CriticalMiss => 0
      }

      val damagedTarget =
        target.copy(creature = target.creature.updateHealth(dmg, damageType, attackResult))

      val udpdatedTarget = attackResult match {
        case CriticalHit | Hit =>
          val acidArrowCondition = AcidArrowCondition()

          val currentConditions = damagedTarget.creature.conditions
          (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
            .set(currentConditions ++ List(acidArrowCondition))(damagedTarget)
        case CriticalMiss | Miss => damagedTarget
      }

      (spellCaster, targets.replace(udpdatedTarget))
    }
  }

  case object Fireball extends MultiTargetSavingThrowSpell {
    val name: String           = "Fireball"
    val damageType: DamageType = Fire

    val school: SchoolOfMagic    = Evocation
    val castingTime: CastingTime = OneActionCast

    val attribute: Attribute           = Dexterity
    val halfDamageOnSave: Boolean      = true
    val spellLevel: SpellLevel         = 3
    val requiresConcentration: Boolean = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (5 + spellLevel) * D8
  }

  case object Shield extends OnHitReaction {
    val name: String = "Shield (spell)"

    def updateAttackOnReaction[_: RS](reactingCreature: Creature, totalAttackRoll: Int): (AttackResult, Creature) = {
      logger.debug(s"${reactingCreature.name} used its reaction to cast $name")

      val updatedConditions = reactingCreature.conditions :+ ShieldBuffCondition
      val reactedCreature = Creature.creatureConditionsLens.set(updatedConditions)(reactingCreature)

      // TODO does this armourClass take the +5 into account ??
      val attackResult = if (totalAttackRoll >= reactedCreature.armourClass) Hit else Miss

      (attackResult, reactedCreature)
    }
  }

  case object ShieldBuffCondition extends StartOfTurnCondition {
    val saveDc: Int = 0
    val turnsLeft: Int = 1
    val missesTurn: Boolean = false
    val handleOnDamage: Boolean = false
    val name: String = "Shield (Buff)"

    def handleStartOfTurn[_: RS](creature: Creature): Creature = {
      val updatedConditions = creature.conditions diff List(ShieldBuffCondition)
      Creature.creatureConditionsLens.set(updatedConditions)(creature)
    }
  }
}
