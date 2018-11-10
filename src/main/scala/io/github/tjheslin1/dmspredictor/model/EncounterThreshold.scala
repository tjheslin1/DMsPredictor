package io.github.tjheslin1.dmspredictor.model

/**
  * Encounter building based on a XP (Dungeon Master's Guide page 82) is a more accurate method of building balanced
  * encounters versus using Challenge Rating (same book, same page).
  */
object EncounterThreshold {

  val easyEncounter = Map(
    LevelOne -> 25
  )
}
