package io.github.tjheslin1.dmspredictor.model

import cats.Eq
import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.classes.{Player, SpellCaster}
import monocle.macros.GenLens
import monocle.{Lens, Optional}

case class Combatant(index: Int, creature: Creature)

object Combatant {

  implicit val combatantEq: Eq[Combatant] = (x: Combatant, y: Combatant) => x.index == y.index

  val creatureLens: Lens[Combatant, Creature] = GenLens[Combatant](_.creature)

  // format: off
  val playerOptional: Optional[Combatant, Player] = Optional[Combatant, Player] {
    case Combatant(_, p: Player) => p.some
    case _                       => none[Player]
  } { player =>
    {
      case combatant @ Combatant(_, _: Player) => creatureLens.set(player)(combatant)
      case c: Combatant                        => c
    }
  }

  val spellCasterOptional: Optional[Combatant, SpellCaster] = Optional[Combatant, SpellCaster] {
    case Combatant(_, sc: SpellCaster) => sc.some
    case _                             => none[SpellCaster]
  } { spellCaster =>
    {
      case combatant @ Combatant(_, _: SpellCaster) => creatureLens.set(spellCaster)(combatant)
      case c: Combatant                             => c
    }
  }
  // format: on
}
