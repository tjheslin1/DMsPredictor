package io.github.tjheslin1.dmspredictor.classes.cleric

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import io.github.tjheslin1.dmspredictor.model.ability.{Ability, AbilityAction, WholeAction}
import io.github.tjheslin1.dmspredictor.model._

object BaseClericAbilities extends LazyLogging {

  def turnUndead(currentOrder: Int)(combatant: Combatant): Ability = new Ability(combatant) {
    val baseCleric = combatant.creature.asInstanceOf[BaseCleric]

    val name: String                 = "Turn Undead"
    val order: Int                   = currentOrder
    val levelRequirement: Level      = LevelTwo
    val abilityAction: AbilityAction = WholeAction

    def conditionMet: Boolean = baseCleric.channelDivinityUsed == false
    def triggerMet(target: Option[Combatant]): Boolean = target match {
      case None                                                               => false
      case Some(targetOfTurn) if targetOfTurn.creature.creatureType == Undead => true
      case _                                                                  => false
    }

    def useAbility[_: RS](target: Option[Combatant]): (Combatant, Option[Combatant]) = {
      logger.debug(s"${baseCleric.name} used $name")

      target match {
        case None => (combatant, none[Combatant])
        case Some(targetOfTurn) => ???
    }
      }

    def update: Creature = BaseCleric.channelDivinityUsedLens.set(true)(baseCleric)
  }
}
