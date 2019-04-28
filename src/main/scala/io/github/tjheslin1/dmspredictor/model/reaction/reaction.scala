package io.github.tjheslin1.dmspredictor.model.reaction

import io.github.tjheslin1.dmspredictor.model._

sealed trait Reaction {
  val name: String
}

trait OnHitReaction extends Reaction {

  def updateAttackOnReaction[_: RS](reactingCreature: Creature,
                                    totalAttackRoll: Int): (AttackResult, Creature)
}

trait OnDamageReaction extends Reaction {

  def updateHealthOnReaction[_: RS](reactingCreature: Creature,
                                    damage: Int,
                                    damageType: DamageType,
                                    attackResult: AttackResult): Creature
}
