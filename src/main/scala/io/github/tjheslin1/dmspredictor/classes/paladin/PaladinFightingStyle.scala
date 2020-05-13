package io.github.tjheslin1.dmspredictor.classes.paladin

sealed trait PaladinFightingStyle extends Product with Serializable

case object Defense             extends PaladinFightingStyle
case object Dueling             extends PaladinFightingStyle
case object GreatWeaponFighting extends PaladinFightingStyle
