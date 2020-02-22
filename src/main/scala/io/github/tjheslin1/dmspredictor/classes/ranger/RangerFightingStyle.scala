package io.github.tjheslin1.dmspredictor.classes.ranger

sealed trait RangerFightingStyle extends Product with Serializable

case object Archery           extends RangerFightingStyle
case object Defense           extends RangerFightingStyle
case object Dueling           extends RangerFightingStyle
case object TwoWeaponFighting extends RangerFightingStyle
