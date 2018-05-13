package io.github.tjheslin1.model

case class BaseStats(strength: Strength,
                dexterity: Dexterity,
                constitution: Constitution,
                wisdom: Wisdom,
                intelligence: Intelligence,
                charisma: Charisma) {
}

object BaseStats {
  def apply(strength: Int,
            dexterity: Int,
            constitution: Int,
            wisdom: Int,
            intelligence: Int,
            charisma: Int): BaseStats = new BaseStats(
    Strength(strength),
    Dexterity(dexterity),
    Constitution(constitution),
    Wisdom(wisdom),
    Intelligence(intelligence),
    Charisma(charisma)
  )
}

sealed trait Stat {
  val score: Int
}

case class Strength(scr: Int) extends Stat {
  val score = scr
}

case class Dexterity(scr: Int) extends Stat {
  val score = scr
}

case class Constitution(scr: Int) extends Stat {
  val score = scr
}

case class Wisdom(scr: Int) extends Stat {
  val score = scr
}

case class Intelligence(scr: Int) extends Stat {
  val score = scr
}

case class Charisma(scr: Int) extends Stat {
  val score = scr
}