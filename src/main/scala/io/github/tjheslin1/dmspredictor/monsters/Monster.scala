package io.github.tjheslin1.dmspredictor.monsters

import io.github.tjheslin1.dmspredictor.model.Creature
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import monocle.Lens

trait Monster extends Creature {

  val challengeRating: Double
}

object Monster {

  val monsterArmourClassLens: Lens[Monster, Int] = Lens[Monster, Int](_.armourClass) { ac =>
    {
      case c: Goblin   => Goblin._armourClass.set(ac)(c)
      case c: Werewolf => Werewolf._armourClass.set(ac)(c)
      case c: Vampire  => Vampire._armourClass.set(ac)(c)

      case _ => throw new NotImplementedError("missing implementation in monsterArmourClassLens for new Monster")
    }
  }
}
