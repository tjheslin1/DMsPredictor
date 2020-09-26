package io.github.tjheslin1.dmspredictor.monsters

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import monocle.Lens

trait Legendary extends Creature {

  val legendaryResistances: Int
}

object Legendary {

  val legendaryResistancesLens: Lens[Legendary, Int] =
    Lens[Legendary, Int](_.legendaryResistances) { legendaryResistances =>
      {
        case c: Lich =>
          Lich._legendaryResistances.set(legendaryResistances)(c)
        case c: Vampire =>
          Vampire._legendaryResistances.set(legendaryResistances)(c)
      }
    }
}
