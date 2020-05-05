package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.ranger.BaseRanger
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.OnWeaponDamageAbility
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

object RangerSpells extends LazyLogging {

  case object HuntersMarkBuffCondition extends PassiveCondition {
    val name                        = "Hunter's Mark (bonus damage)"
    val missesTurn                  = false
    val benefitsFromHigherSpellSlot = false

    val turnsLeft = Integer.MAX_VALUE

    def decrementTurnsLeft(): Condition = this
  }

  case object HuntersMark extends SelfBuffSpell {
    val name              = "Hunter's Mark"
    val selfBuffCondition = HuntersMarkBuffCondition

    val school: SchoolOfMagic    = Divination
    val castingTime: CastingTime = BonusActionCast

    val spellLevel: SpellLevel      = 1
    val requiresConcentration       = true
    val benefitsFromHigherSpellSlot = false
  }

  def huntersMarkOnWeaponDamageAbility(
      currentOrder: Int
  )(combatant: Combatant): OnWeaponDamageAbility =
    new OnWeaponDamageAbility(combatant) {

      val name: String            = "Hunters Mark extra damage"
      val order: Int              = currentOrder
      val levelRequirement: Level = LevelTwo

      def damage[_: RS](): Int = 1 * D6

      def triggerMet(others: List[Combatant]): Boolean = true

      def conditionMet: Boolean =
        combatant.creature match {
          case spellCastingPlayer: Player with SpellCaster =>
            spellCastingPlayer.level >= levelRequirement &&
              spellCastingPlayer.conditions.contains(HuntersMarkBuffCondition) &&
              spellCastingPlayer.spellsKnown.exists {
                case (_, spell) => spell.name == HuntersMark.name
              }
          case spellCaster: SpellCaster =>
            spellCaster.conditions.contains(HuntersMarkBuffCondition) &&
              spellCaster.spellsKnown.exists {
                case (_, spell) => spell.name == HuntersMark.name
              }
          case _ => false
        }

      def update: Creature = combatant.creature
    }
}
