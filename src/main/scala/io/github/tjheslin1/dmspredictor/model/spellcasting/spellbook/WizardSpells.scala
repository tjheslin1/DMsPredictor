package io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook

import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting._
import io.github.tjheslin1.dmspredictor.util.IntOps._

import scala.util.Random

object WizardSpells {

  case object ChromaticOrb extends Spell {

    private val possibleDamageTypes = List(Acid, Cold, Fire, Lightning, Poison, Thunder)

    val school: SchoolOfMagic                = Evocation
    val castingTime: CastingTime             = OneAction
    val spellOffenseStyle: SpellOffenseStyle = RangedSpellAttack
    val damageType: DamageType               = possibleDamageTypes(Random.nextInt(possibleDamageTypes.size))
    val spellLevel: SpellLevel               = 1

    def damage(implicit rollStrategy: RollStrategy): Int = 3 * D8

  }
}
