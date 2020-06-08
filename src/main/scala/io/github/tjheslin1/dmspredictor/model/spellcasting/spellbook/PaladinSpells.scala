package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.Cleric
import io.github.tjheslin1.dmspredictor.classes.fighter.{Champion, Fighter}
import io.github.tjheslin1.dmspredictor.classes.paladin.Paladin
import io.github.tjheslin1.dmspredictor.classes.ranger.{Hunter, Ranger}
import io.github.tjheslin1.dmspredictor.classes.rogue.Rogue
import io.github.tjheslin1.dmspredictor.classes.wizard.Wizard
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.{Condition, PassiveCondition}
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.monsters.{Goblin, Werewolf, Zombie}
import io.github.tjheslin1.dmspredictor.monsters.lich.Lich
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire

object PaladinSpells {

  // TODO make an OnHitCondition
  case class BlessCondition(turnsLeft: Int = 10) extends PassiveCondition {
    val name       = "Bless (Condition)"
    val missesTurn = false

    def decrementTurnsLeft(): Condition = BlessCondition(turnsLeft - 1)
  }

  case object Bless extends MultiTargetBuffSpell {
    val name          = "Bless"
    val buffCondition = BlessCondition()

    val castingTime                 = OneActionCast
    val requiresConcentration       = true
    val benefitsFromHigherSpellSlot = true
    val school                      = Enchantment

    val spellLevel: SpellLevel = 1
    val affectedTargets        = 2 + spellLevel

    val buffTargetsPriority: Ordering[Combatant] = (x: Combatant, y: Combatant) =>
      if (creatureOrder(x.creature) == creatureOrder(y.creature)) 0
      else if (creatureOrder(x.creature) < creatureOrder(y.creature)) -1
      else 1

    def creatureOrder(creature: Creature): Int =
      creature match {
        case _: Barbarian => 1
        case _: Berserker => 1
        case _: Fighter   => 2
        case _: Champion  => 2
        case _: Rogue     => 3
        case _: Ranger    => 4
        case _: Hunter    => 4
        case _: Paladin   => 5
        case _: Wizard    => 6
        case _: Cleric    => 7

        case _: Lich     => 8
        case _: Vampire  => 9
        case _: Werewolf => 10
        case _: Goblin   => 11
        case _: Zombie   => 12
      }
  }
}
