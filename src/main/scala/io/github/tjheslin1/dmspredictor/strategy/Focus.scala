package io.github.tjheslin1.dmspredictor.strategy

sealed trait Focus extends Product with Serializable

case object LowestFirst extends Focus
case object RandomFocus extends Focus
