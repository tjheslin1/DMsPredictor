package io.github.tjheslin1.dmspredictor.strategy

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Combatant

import scala.util.{Random => JRandom}

sealed trait Focus extends Product with Serializable

case object LowestFirst extends Focus
case object RandomFocus extends Focus

object Focus {

  def nextToFocus(combatantsToAttack: List[Combatant], focus: Focus): Option[Combatant] = {
    val consciousCombatants = combatantsToAttack.filter(_.creature.isConscious)
    focus match {
      case LowestFirst => consciousCombatants.sortBy(_.creature.health).headOption
      case RandomFocus =>
        if (consciousCombatants.isEmpty) None
        else consciousCombatants(JRandom.nextInt(consciousCombatants.size)).some
    }
  }
}