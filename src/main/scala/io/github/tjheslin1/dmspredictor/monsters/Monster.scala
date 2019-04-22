package io.github.tjheslin1.dmspredictor.monsters

import io.github.tjheslin1.dmspredictor.model.Modifier.mod
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.reaction.{OnDamageReaction, OnHitReaction}
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import monocle.Lens

trait Monster extends Creature {

  val challengeRating: Double

  val savingThrowScores: Map[Attribute, Int]

  val reactionOnHit: Option[OnHitReaction]       = None
  val reactionOnDamage: Option[OnDamageReaction] = None
}

object Monster {

  def defaultSavingThrowScores(monster: Monster): Map[Attribute, Int] = Map(
    Strength     -> mod(monster.stats.strength),
    Dexterity    -> mod(monster.stats.dexterity),
    Constitution -> mod(monster.stats.constitution),
    Wisdom       -> mod(monster.stats.wisdom),
    Intelligence -> mod(monster.stats.intelligence),
    Charisma     -> mod(monster.stats.charisma)
  )

  val monsterArmourClassLens: Lens[Monster, Int] = Lens[Monster, Int](_.armourClass) { ac =>
    {
      case c: Goblin   => Goblin._armourClass.set(ac)(c)
      case c: Werewolf => Werewolf._armourClass.set(ac)(c)
      case c: Vampire  => Vampire._armourClass.set(ac)(c)

      case _ =>
        throw new NotImplementedError(
          "missing implementation in monsterArmourClassLens for new Monster")
    }
  }
}
