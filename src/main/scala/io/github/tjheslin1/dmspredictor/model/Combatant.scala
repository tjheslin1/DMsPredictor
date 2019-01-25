package io.github.tjheslin1.dmspredictor.model

import cats.Eq
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.Player
import monocle.macros.GenLens
import monocle.{Lens, Optional}

case class Combatant(index: Int, creature: Creature)

object Combatant {

  implicit val combatantEq: Eq[Combatant] = (x: Combatant, y: Combatant) => x.index == y.index

  val creatureLens: Lens[Combatant, Creature] = GenLens[Combatant](_.creature)

  val playerPrism: Optional[Combatant, Creature with Player] = Optional[Combatant, Creature with Player] {
    case Combatant(_, p: Creature with Player) => p.some
    case _                                     => none[Creature with Player]
  } { playerCreature =>
    {
      case combatant @ Combatant(_, _: Creature with Player) => combatant.copy(creature = playerCreature)
      case c: Combatant                                      => c
    }
  }
}
