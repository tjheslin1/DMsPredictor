package io.github.tjheslin1.dmspredictor.monsters

import io.github.tjheslin1.dmspredictor.model.Creature

trait Monster extends Creature {

  val challengeRating: Double
}
