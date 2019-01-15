package io.github.tjheslin1.dmspredictor.model

import cats.Eq
import monocle.Lens
import monocle.macros.GenLens

case class Combatant(index: Int, creature: Creature)

object Combatant {

  implicit val combatantEq: Eq[Combatant] = (x: Combatant, y: Combatant) => x.index == y.index

  val creatureLens: Lens[Combatant, Creature] = GenLens[Combatant](_.creature)
}
