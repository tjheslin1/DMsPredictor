package io.github.tjheslin1.dmspredictor.classes.rogue

import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue.sneakAttackDamage
import io.github.tjheslin1.dmspredictor.model.Actions._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.ability._
import io.github.tjheslin1.dmspredictor.strategy.Focus
import io.github.tjheslin1.dmspredictor.strategy.Focus.nextToFocus
import io.github.tjheslin1.dmspredictor.strategy.Target.monsters
import io.github.tjheslin1.dmspredictor.util.IntOps._
import io.github.tjheslin1.dmspredictor.util.ListOps._

object BaseRogueAbilities extends LazyLogging {

  def sneakAttack(currentPriority: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name: String                 = "Sneak Attack"
    val order: Int                   = currentPriority
    val levelRequirement: Level      = LevelOne
    val abilityAction: AbilityAction = WholeAction

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean =
      baseRogue.attackStatus == Advantage || baseRogue.hiddenFrom.isEmpty == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      nextToFocus(baseRogue.hiddenFrom, focus) match {
        case None => (combatant, others)
        case Some(target) =>
          val sneakAttackingRogue =
            (Combatant.creatureLens composeLens Creature.creatureAttackStatusLens)
              .set(Advantage)(combatant)

          attack(sneakAttackingRogue, sneakAttackingRogue.creature.weapon, target) match {
            case CriticalMiss | Miss => (combatant, others)
            case attackHitResult =>

              val sneakAttackDmg = {
                def damage: Int = sneakAttackDamage(baseRogue.level) * BaseRogue.SneakAttackDice

                attackHitResult match {
                  case CriticalHit => damage + damage
                  case Hit => damage
                }
              }

              val (updatedRogue, updatedTarget, updatedOthers) =
                resolveDamage(sneakAttackingRogue,
                              target,
                              others,
                              sneakAttackingRogue.creature.weapon,
                  attackHitResult,
                              sneakAttackDmg)

              logger.debug(s"${baseRogue.name} dealt $sneakAttackDmg sneak attack damage")

              (updatedRogue, updatedOthers.replace(updatedTarget))
          }

      }
    }

    def update: Creature = BaseRogue.hiddenFromLens.set(List.empty[Combatant])(baseRogue)
  }

  /**
    * Assumed to be using Cunning Action.
    */
  def hide(currentPriority: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseRogue = combatant.creature.asInstanceOf[BaseRogue]

    val name: String = "Hide (Rogue)"
    val order: Int   = currentPriority

    val levelRequirement: Level = LevelTwo

    val abilityAction: AbilityAction = BonusAction

    def triggerMet(others: List[Combatant]): Boolean = true
    def conditionMet: Boolean                        = baseRogue.bonusActionUsed == false

    def useAbility[_: RS](others: List[Combatant], focus: Focus): (Combatant, List[Combatant]) = {
      logger.debug(s"${combatant.creature.name} used $name")

      val hideDc = D20.roll() + baseRogue.skills.stealth

      val updatedRogue = monsters(others).foldLeft(baseRogue) {
        case (hidingRogue, enemy) =>
          if (enemy.creature.passivePerception >= hideDc) hidingRogue
          else {
            val updatedEnemiesHiddenFrom = hidingRogue.hiddenFrom ++ List(enemy)

            logger.debug(s"${baseRogue.name} is hidden from ${enemy.creature.name}")

            BaseRogue.hiddenFromLens.set(updatedEnemiesHiddenFrom)(baseRogue)
          }
      }

      val updatedCombatant = Combatant.creatureLens.set(updatedRogue)(combatant)

      (updatedCombatant, others)
    }

    def update: Creature = Player.playerBonusActionUsedLens.set(true)(baseRogue)
  }
}
