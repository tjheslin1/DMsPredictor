package io.github.tjheslin1.dmspredictor.strategy

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.rogue.BaseRogue
import io.github.tjheslin1.dmspredictor.model.{Combatant, PlayerCharacter}
import io.github.tjheslin1.dmspredictor.strategy.Target.players

import scala.util.{Random => JRandom}

sealed trait Focus extends Product with Serializable

case object LowestFirst   extends Focus
case object RandomFocus   extends Focus
case object PlayerHealing extends Focus

object Focus {

  def nextToFocus(combatant: Combatant,
                  targets: List[Combatant],
                  focus: Focus): Option[Combatant] = {
    val consciousCombatants = targets.filter(_.creature.isConscious)

    val visibleCombatants = if (combatant.creature.creatureType != PlayerCharacter) {
      consciousCombatants.filter {
        case Combatant(_, rogue: BaseRogue) => rogue.hiddenFrom.contains(combatant) == false
        case _                              => true
      }
    } else consciousCombatants

    focus match {
      case LowestFirst => visibleCombatants.sortBy(_.creature.health).headOption
      case RandomFocus =>
        if (visibleCombatants.isEmpty) None
        else visibleCombatants(JRandom.nextInt(visibleCombatants.size)).some
      case PlayerHealing =>
        players(targets).sortBy(_.creature.health).headOption
    }
  }
}
