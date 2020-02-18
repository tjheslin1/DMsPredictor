package io.github.tjheslin1.dmspredictor.classes.ranger

sealed trait RangerFightingStyle extends Product with Serializable

case object Dueling extends RangerFightingStyle