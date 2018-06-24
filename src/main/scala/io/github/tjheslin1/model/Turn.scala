package io.github.tjheslin1.model

import io.github.tjheslin1.simulation._

class Turn(initiatives: List[(Creature, Int)])(implicit rollStrategy: RollStrategy) {

  val (pcs, monsters) = initiatives.partition(c => c._1.isInstanceOf[PlayerCharacter])

  def run = {
    for ((creature, _) <- initiatives) {

      if (creature.isInstanceOf[PlayerCharacter]) {
        val mob = monsters.head._1

        if (creature.attack(mob) == Success)
          creature.resolveDamage(creature.weapon, mob) //s"${creature.name} vs ${mob.name}"
        else Unresolved
      } else {

        val pc = pcs.head._1

        if (creature.attack(pc) == Success)
          creature.resolveDamage(creature.weapon, pc) //s"${creature.name} vs ${pc.name}"
        else Unresolved
      }
    }
  }
}

object Turn {

  def apply(initiatives: List[(Creature, Int)])(implicit rollStrategy: RollStrategy): Turn = new Turn(initiatives)
}
