package io.github.tjheslin1.dmspredictor.strategy

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.model.Combatant
import io.github.tjheslin1.dmspredictor.strategy.Target.players

import scala.util.{Random => JRandom}

sealed trait Focus extends Product with Serializable

case object LowestFirst   extends Focus
case object RandomFocus   extends Focus
case object PlayerHealing extends Focus

object Focus {

  def nextToFocus(targets: List[Combatant], focus: Focus): Option[Combatant] = {
    val consciousCombatants = targets.filter(_.creature.isConscious)
    focus match {
      case LowestFirst => consciousCombatants.sortBy(_.creature.health).headOption
      case RandomFocus =>
        if (consciousCombatants.isEmpty) None
        else consciousCombatants(JRandom.nextInt(consciousCombatants.size)).some
      case PlayerHealing =>
        players(targets).sortBy(_.creature.health).headOption
    }
  }
}
