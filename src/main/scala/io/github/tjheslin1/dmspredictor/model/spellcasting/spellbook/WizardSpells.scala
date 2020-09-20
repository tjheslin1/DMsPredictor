package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.SpellCaster
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition._
import io.github.tjheslin1.dmspredictor.model.reaction.OnHitReaction
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellSaveDc
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.Zombie
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object WizardSpells extends LazyLogging {

  case object FireBolt extends SingleTargetAttackSpell {
    val name       = "Fire Bolt"
    val damageType = Fire

    val school                      = Evocation
    val castingTime                 = OneActionCast
    val spellTargetStyle            = RangedSpellAttack
    val spellLevel: SpellLevel      = 0
    val benefitsFromHigherSpellSlot = false
    val halfDamageOnMiss            = false

    //@format: off
    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      spellCaster.spellCastingLevel.value match {
        case lvl if lvl >= 17 => 4 * D10
        case lvl if lvl >= 11 => 3 * D10
        case lvl if lvl >= 5  => 2 * D10
        case _                => 1 * D10
      }
    //@format: on
  }

  case object MagicMissile extends SingleTargetAttackSpell {
    val name                   = "Magic Missile"
    val damageType: DamageType = Force

    val school                      = Evocation
    val castingTime                 = OneActionCast
    val spellTargetStyle            = RangedSpellAttack
    val spellLevel: SpellLevel      = 1
    val benefitsFromHigherSpellSlot = true
    val halfDamageOnMiss            = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = {
      val numberOfDarts = 2 + spellLevel

      (numberOfDarts * D4) + numberOfDarts
    }

    /*
    Magic Missile always Hits
     */
    override def effect[_: RS](
        spellCaster: SpellCaster,
        spellLevel: SpellLevel,
        targets: List[Combatant]
    ): (SpellCaster, List[Combatant]) = {
      val target = targets.head
      logger.debug(s"casting $name")

      val damagedTarget = target.copy(
        creature = target.creature.updateHealth(damage(spellCaster, spellLevel), damageType, Hit)
      )

      (spellCaster, targets.replace(damagedTarget))
    }
  }

  case object AcidArrow extends SingleTargetAttackSpell {
    val name       = "Acid Arrow"
    val damageType = Acid

    val school                      = Evocation
    val castingTime                 = OneActionCast
    val spellLevel: SpellLevel      = 2
    val benefitsFromHigherSpellSlot = true
    val halfDamageOnMiss            = true

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (spellLevel + 2) * D4

    override def additionalEffect[_: RS](target: Combatant, attackResult: AttackResult): Combatant =
      attackResult match {
        case CriticalHit | Hit =>
          val currentConditions  = target.creature.conditions
          val acidArrowCondition = AcidArrowCondition(spellLevel)

          (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
            .set(currentConditions :+ acidArrowCondition)(target)
        case CriticalMiss | Miss =>
          target
      }
  }

  case object Fireball extends MultiTargetSavingThrowSpell {
    val name: String           = "Fireball"
    val damageType: DamageType = Fire

    val school: SchoolOfMagic    = Evocation
    val castingTime: CastingTime = OneActionCast

    val attribute: Attribute        = Dexterity
    val halfDamageOnSave            = true
    val spellLevel: SpellLevel      = 3
    val benefitsFromHigherSpellSlot = true

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (5 + spellLevel) * D8
  }

  case object ShieldSpell extends OnHitReaction {
    val name = "Shield (spell)"

    def updateAttackOnReaction[_: RS](
        reactingCreature: Creature,
        totalAttackRoll: Int
    ): (AttackResult, Creature) = {
      logger.debug(s"${reactingCreature.name} used its reaction to cast $name")

      if (totalAttackRoll < reactingCreature.armourClass)
        (Miss, reactingCreature)
      else {
        val spellCaster = reactingCreature.asInstanceOf[SpellCaster]
        if (spellCaster.spellSlots.firstLevel.count > 0) {

          val reactedCreature = {
            val updatedSpellSlots = spellCaster.spellSlots.copy(
              firstLevel = FirstLevelSpellSlots(spellCaster.spellSlots.firstLevel.count - 1)
            )

            val updatedSpellCaster = SpellCaster.spellSlotsLens.set(updatedSpellSlots)(spellCaster)

            val updatedConditions = reactingCreature.conditions :+ ShieldBuffCondition()
            val conditionUpdated =
              Creature.creatureConditionsLens.set(updatedConditions)(updatedSpellCaster)

            Creature.creatureReactionUsedLens.set(true)(conditionUpdated)
          }

          val attackResult =
            if (totalAttackRoll >= reactedCreature.armourClass)
              Hit
            else
              Miss

          (attackResult, reactedCreature)
        } else
          (Hit, reactingCreature)
      }
    }
  }

  case class ShieldBuffCondition(turnsLeft: Int = 1) extends PassiveCondition {
    val missesTurn = false
    val name       = "Shield (Buff)"

    def decrementTurnsLeft(): Condition = ShieldBuffCondition(turnsLeft = 0)
  }

  case object Blight extends SingleTargetSavingThrowSpell {
    val name                     = "Blight"
    val school                   = Necromancy
    val castingTime: CastingTime = OneActionCast
    val spellLevel: SpellLevel   = 4

    val savingThrowAttribute   = Constitution
    val halfDamageOnSave       = true
    val damageType: DamageType = Necrotic

    val benefitsFromHigherSpellSlot = true

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      (spellLevel.value * D8) + (4 * D8)
  }

  case object Disintegrate extends SingleTargetSavingThrowSpell {
    val name        = "Disintegrate"
    val school      = Transmutation
    val castingTime = OneActionCast
    val spellLevel  = 6

    val savingThrowAttribute = Dexterity
    val halfDamageOnSave     = false
    val damageType           = Force

    val benefitsFromHigherSpellSlot = true

    //@format: off
    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int =
      spellLevel.value match {
        case 6 => (10 * D6) + 40
        case 7 => (13 * D6) + 40
        case 8 => (16 * D6) + 40
        case 9 => (19 * D6) + 40

        case _ =>
          throw new IllegalArgumentException(
            s"Invalid spell level. Expected 6 or higher but got: ${spellLevel.value}"
          )
      }
    //@format: on

    override def additionalEffect[_: RS](
        spellCaster: SpellCaster,
        target: Combatant,
        others: List[Combatant],
        savingThrowPassed: Boolean
    ): (SpellCaster, Combatant, List[Combatant]) =
      if (target.creature.health <= 0) {
        val updatedTarget =
          (Combatant.creatureLens composeLens Creature.creatureIsAliveLens)
            .set(false)(target)

        (spellCaster, updatedTarget, others)
      } else
        (spellCaster, target, others)
  }

  case object FingerOfDeath extends SingleTargetSavingThrowSpell {
    val name        = "Finger of Death"
    val school      = Necromancy
    val castingTime = OneActionCast
    val spellLevel  = 6

    val savingThrowAttribute = Constitution
    val halfDamageOnSave     = true
    val damageType           = Necrotic

    val benefitsFromHigherSpellSlot = false

    def damage[_: RS](spellCaster: SpellCaster, spellLevel: SpellLevel): Int = (7 * D8) + 30

    override def additionalEffect[_: RS](
        spellCaster: SpellCaster,
        target: Combatant,
        others: List[Combatant],
        savingThrowPassed: Boolean
    ): (SpellCaster, Combatant, List[Combatant]) =
      if (savingThrowPassed == false && target.creature.isAlive == false) {
        val countOfSpellCasterAndTarget = 2
        val zombiesCombatIndex          = others.size + countOfSpellCasterAndTarget + 1

        val risenZombie = Combatant(
          zombiesCombatIndex,
          Zombie.withName(s"Lich's Zombie (formerly ${target.creature.name})")
        )

        (spellCaster, target, others :+ risenZombie)
      } else
        (spellCaster, target, others)
  }

  case object PowerWordStun extends SingleTargetInstantEffectSpell {
    val name = "Power Word Stun"

    val school                      = Enchantment
    val castingTime                 = OneActionCast
    val spellLevel                  = 8
    val benefitsFromHigherSpellSlot = false

    def instantEffect(
        spellCaster: SpellCaster,
        spellLevel: SpellLevel,
        target: Combatant
    ): (SpellCaster, Combatant) =
      if (target.creature.health <= 150) {

        val updatedConditions = target.creature.conditions :+ Stunned(spellSaveDc(spellCaster))

        val updatedTarget =
          (Combatant.creatureLens composeLens Creature.creatureConditionsLens)
            .set(updatedConditions)(target)

        logger.debug(s"${updatedTarget.creature.name} is stunned")

        (spellCaster, updatedTarget)
      } else
        (spellCaster, target)
  }

  case object PowerWordKill extends SingleTargetInstantEffectSpell {
    val name = "Power Word Kill"

    val school                      = Enchantment
    val castingTime                 = OneActionCast
    val spellLevel                  = 9
    val benefitsFromHigherSpellSlot = false

    def instantEffect(
        spellCaster: SpellCaster,
        spellLevel: SpellLevel,
        target: Combatant
    ): (SpellCaster, Combatant) =
      if (target.creature.health <= 100) {

        val zeroHealthTarget =
          (Combatant.creatureLens composeLens Creature.creatureHealthLens)
            .set(0)(target)

        val deadTarget =
          (Combatant.creatureLens composeLens Creature.creatureIsAliveLens)
            .set(false)(zeroHealthTarget)

        logger.debug(s"${deadTarget.creature.name} is dead")

        (spellCaster, deadTarget)
      } else
        (spellCaster, target)
  }
}
