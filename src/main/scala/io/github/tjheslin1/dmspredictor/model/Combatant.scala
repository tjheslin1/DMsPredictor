package io.github.tjheslin1.dmspredictor.model

import monocle.Lens
import monocle.macros.GenLens

case class Combatant(index: Int, creature: Creature)

object Combatant {

  val creatureLens: Lens[Combatant, Creature] = GenLens[Combatant](_.creature)
}
