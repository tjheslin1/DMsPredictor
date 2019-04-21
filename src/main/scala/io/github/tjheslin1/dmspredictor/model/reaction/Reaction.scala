package io.github.tjheslin1.dmspredictor.model.reaction

import io.github.tjheslin1.dmspredictor.model._

sealed trait Reaction {

  def effect(): Creature
}

abstract class OnHitReaction extends Reaction {}
abstract class OnDamageReaction(damage: Int, damageType: DamageType) extends Reaction {}
