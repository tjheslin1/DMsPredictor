package io.github.tjheslin1.dmspredictor.classes.cleric

import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.CoreAbilities._
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots.highestSpellSlotAvailable
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model.spellcasting.Spell.spellOfLevelOrBelow
import io.github.tjheslin1.dmspredictor.model.spellcasting.{HealingSpell, SpellLevel}
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Target.players
import io.github.tjheslin1.dmspredictor.util.ListOps._

object LifeClericAbilities extends LazyLogging {

  def discipleOfLifeBonusHealing(spellLevel: SpellLevel): Int = 2 + spellLevel

  def discipleOfLife(currentOrder: Int)(combatant: Combatant): Ability =
    new Ability(combatant) {
      val lifeCleric = combatant.creature.asInstanceOf[Cleric]

      val name: String = "Disciple of Life"
      val order: Int   = currentOrder

      val levelRequirement: Level      = LevelOne
      val abilityAction: AbilityAction = WholeAction

      def triggerMet(others: List[Combatant]): Boolean = healingSpellTriggerMet(others)

      def conditionMet: Boolean = healingSpellConditionMet(lifeCleric)

      def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
        logger.debug(s"${lifeCleric.name} used $name")

        if (lifeCleric.abilities
              .map(_(combatant).name)
              .contains(CastSingleTargetHealingSpellName)) {

          val optSpell = highestSpellSlotAvailable(lifeCleric.spellSlots) match {
            case None => None
            case Some(spellSlot) =>
              spellOfLevelOrBelow(lifeCleric, HealingSpell, spellSlot.spellLevel)
          }

          optSpell.fold((combatant, others)) { healingSpell =>
            castSingleTargetHealingSpell(
              order,
              bonusHealing = discipleOfLifeBonusHealing(healingSpell.spellLevel))(combatant)
              .useAbility(others, focus)
          }
        } else
          (combatant, others)
      }

      def update: Creature = lifeCleric
    }

  def preserveLifeHealing(level: Level): Int = 5 * level

  def preserveLife(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

    val name: String = "Preserve Life"
    val order: Int   = currentOrder

    val levelRequirement: Level      = LevelTwo
    val abilityAction: AbilityAction = WholeAction

    def triggerMet(others: List[Combatant]): Boolean = {
      val allies = players(others)
      val alliesBelowHalfHealth =
        allies.count(player => player.creature.health <= (player.creature.maxHealth / 2))

      allies.nonEmpty && alliesBelowHalfHealth >= Math.ceil(1 + allies.size / 2)
    }

    def conditionMet: Boolean =
      baseCleric.level >= levelRequirement && baseCleric.channelDivinityUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${baseCleric.name} used $name")

      val damagedAllies = players(others)
        .filter(player => player.creature.health < player.creature.maxHealth)

      val healingPool = preserveLifeHealing(baseCleric.level)

      val updatedAllies = restoreHealthUsingPool(healingPool, damagedAllies)

      (combatant, others.replace(updatedAllies))
    }

    def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
  }

  def restoreHealthUsingPool(pool: Int, damagedAllies: List[Combatant]): List[Combatant] =
    if (pool <= 0 || damagedAllies.isEmpty) damagedAllies
    else {
      damagedAllies
        .sortBy(_.creature.health)
        .find(player => player.creature.health < (player.creature.maxHealth / 2))
        .fold(damagedAllies) { targetAlly =>
          val maxRestoredHealth = (targetAlly.creature.maxHealth / 2) - targetAlly.creature.health
          val healthToRestore   = Math.min(pool, maxRestoredHealth)

          val healedAlly = (Combatant.creatureLens composeLens Creature.creatureHealthLens)
            .set(targetAlly.creature.health + healthToRestore)(targetAlly)

          logger.debug(s"${healedAlly.creature.name} was healed for $healthToRestore")

          val updatedPool = pool - healthToRestore
          restoreHealthUsingPool(updatedPool, damagedAllies.replace(healedAlly))
        }
    }
}
