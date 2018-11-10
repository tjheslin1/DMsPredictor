package io.github.tjheslin1.strategy

sealed trait Focus extends Product with Serializable

case object LowestFirst extends Focus
case object Random      extends Focus
