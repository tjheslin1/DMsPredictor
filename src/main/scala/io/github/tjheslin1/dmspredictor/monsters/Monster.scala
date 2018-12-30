package io.github.tjheslin1.dmspredictor.monsters

import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.strategy.ClassAbilities

trait Monster

object Monster {

  implicit val monsterAbilities: ClassAbilities[Monster] = new ClassAbilities[Monster] {
    def abilities: List[CreatureAbility[Monster]] = List.empty[CreatureAbility[Monster]]
  }
}
