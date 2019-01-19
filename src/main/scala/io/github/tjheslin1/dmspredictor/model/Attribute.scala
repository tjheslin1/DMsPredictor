package io.github.tjheslin1.dmspredictor.model

sealed trait Attribute extends Product with Serializable

case object Strength     extends Attribute
case object Dexterity    extends Attribute
case object Constitution extends Attribute
case object Wisdom       extends Attribute
case object Intelligence extends Attribute
case object Charisma     extends Attribute
