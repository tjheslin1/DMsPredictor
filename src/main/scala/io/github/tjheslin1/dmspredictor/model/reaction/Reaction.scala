package io.github.tjheslin1.dmspredictor.model.reaction

import io.github.tjheslin1.dmspredictor.model.Creature

sealed trait Reaction {

  def effect(): Creature
}

abstract class OnDamageReaction extends Reaction {}
