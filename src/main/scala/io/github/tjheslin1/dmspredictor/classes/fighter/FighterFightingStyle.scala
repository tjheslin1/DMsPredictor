package io.github.tjheslin1.dmspredictor.classes.fighter

sealed trait FighterFightingStyle extends Product with Serializable

case object Archery             extends FighterFightingStyle
case object Defense             extends FighterFightingStyle
case object Dueling             extends FighterFightingStyle
case object GreatWeaponFighting extends FighterFightingStyle
case object Protection          extends FighterFightingStyle
case object TwoWeaponFighting   extends FighterFightingStyle
