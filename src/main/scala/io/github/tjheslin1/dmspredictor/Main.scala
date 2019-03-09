package io.github.tjheslin1.dmspredictor

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.barbarian._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter.SpellSlots
import io.github.tjheslin1.dmspredictor.equipment.armour.ChainShirt
import io.github.tjheslin1.dmspredictor.equipment.weapons.{Greatsword, Shortsword}
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.FirstLevelSpellSlot
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.SacredFlame
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

//  val creatures: List[Creature] =
//    List(Fighter.levelOneFighter(), Goblin.levelOneGoblin(), Goblin.levelOneGoblin())

  val clericHp = BaseCleric.calculateHealth(LevelFive, 10)
  val cleric = Cleric(
    LevelFive,
    clericHp,
    clericHp,
    BaseStats(10, 10, 10, 14, 14, 14),
    Shortsword,
    SacredFlame.some,
    SpellSlots(FirstLevelSpellSlot(2)),
    Cleric.standardClericSpellList,
    channelDivinityUsed = false,
    ChainShirt,
    None,
    Cleric.standardClericAbilities,
    proficiencyBonus = 2,
    name = "Cleric"
  )

  val barbarianHp = BaseBarbarian.calculateHealth(LevelFive, 10)
  val barbarian = Berserker(
    LevelFive,
    barbarianHp,
    barbarianHp,
    BaseStats(14, 14, 14, 10, 10, 10),
    Greatsword,
    4,
    proficiencyBonus = 2,
    name = "Berserker"
  )

  val creatures = List(cleric, barbarian, Vampire(144, 144, name = "Vampire"))

  val simulation = "Cleric and Barbarian vs Vampire"
  val (losses, wins) =
    SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, 1)

  logger.debug(s"$simulation simulation started")
  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show(title = simulation)
}
