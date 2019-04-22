package util

import cats.syntax.option._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour.{Armour, NoArmour}
import io.github.tjheslin1.dmspredictor.model.AdjustedDamage.adjustedDamage
import io.github.tjheslin1.dmspredictor.model.BaseStats.Stat
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.condition.Condition
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.util.NameGenerator
import monocle.Lens
import monocle.macros.{GenLens, Lenses}
import util.TestMonster.defaultScores

@Lenses("_") case class TestMonster(health: Int,
                                    maxHealth: Int,
                                    stats: BaseStats,
                                    armourClass: Int,
                                    baseWeapon: Weapon,
                                    armour: Armour = NoArmour,
                                    offHand: Option[Equipment] = none[Equipment],
                                    resistances: List[DamageType] = List.empty,
                                    immunities: List[DamageType] = List.empty,
                                    abilities: List[CombatantAbility] = List.empty,
                                    conditions: List[Condition] = List.empty,
                                    reactionUsed: Boolean = false,
                                    attackStatus: AttackStatus = Regular,
                                    defenseStatus: AttackStatus = Regular,
                                    turnResetTracker: Unit => Unit = () => _,
                                    creatureType: CreatureType = Humanoid,
                                    challengeRating: Double = 1,
                                    perceptionScore: Int = 0,
                                    stealthScore: Int = 0,
                                    savingThrowScores: Map[Attribute, Int] = defaultScores,
                                    name: String = NameGenerator.randomName)
    extends Monster {

  val skills: Skills = Skills(perceptionScore, stealthScore)

  def weapon[_: RS]: Weapon = baseWeapon

  def updateHealth[_: RS](dmg: Int, damageType: DamageType, attackResult: AttackResult): Creature =
    copy(health = Math.max(0, health - adjustedDamage(dmg, damageType, this)))

  def scoresCritical(roll: Int): Boolean = roll == 20

  def resetStartOfTurn(): Creature = {
    turnResetTracker()
    this
  }
}

object TestMonster {

  val defaultScores = Map(
    Strength     -> 0,
    Dexterity    -> 0,
    Constitution -> 0,
    Wisdom       -> 0,
    Intelligence -> 0,
    Charisma     -> 0
  )

  val strengthLens: Lens[TestMonster, Stat]  = _stats composeLens GenLens[BaseStats](_.strength)
  val dexterityLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.dexterity)
  val constitutionLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](
    _.constitution)
  val wisdomLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.wisdom)
  val intelligenceLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](
    _.intelligence)
  val charismaLens: Lens[TestMonster, Stat] = _stats composeLens GenLens[BaseStats](_.charisma)
}
