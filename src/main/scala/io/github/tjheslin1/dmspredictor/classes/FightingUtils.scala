package io.github.tjheslin1.dmspredictor.classes

import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.model._

object FightingUtils {

  def duelingFightingStyleConditionsMet[T](
      weapon: Weapon,
      offHand: Option[Equipment],
      fightingStyles: List[T],
      dueling: T
  ): Boolean =
    offHand match {
      case Some(_: Weapon) => false
      case _               => weapon.twoHanded == false && fightingStyles.contains(dueling)
    }
}
