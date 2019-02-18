package io.github.tjheslin1.dmspredictor

import cats.syntax.option._
import com.typesafe.scalalogging.LazyLogging
import eu.timepit.refined.auto._
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter.{Fighter, SpellSlots}
import io.github.tjheslin1.dmspredictor.equipment.armour.ChainShirt
import io.github.tjheslin1.dmspredictor.equipment.weapons.Shortsword
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.model.spellcasting.FirstLevelSpellSlot
import io.github.tjheslin1.dmspredictor.model.spellcasting.spellbook.ClericSpells.{
  GuidingBolt,
  SacredFlame
}
import io.github.tjheslin1.dmspredictor.monsters.Goblin
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

object Main extends App with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

//  val creatures: List[Creature] =
//    List(Fighter.levelOneFighter(), Goblin.levelOneGoblin(), Goblin.levelOneGoblin())

  val clericHp = BaseCleric.calculateHealth(LevelOne, 10) + 20
  val cleric = Cleric(
    LevelOne,
    clericHp,
    clericHp,
    BaseStats(10, 10, 10, 10, 10, 10),
    Shortsword,
    SacredFlame.some,
    Map(GuidingBolt.spellLevel -> GuidingBolt),
    SpellSlots(FirstLevelSpellSlot(2)),
    ChainShirt,
    None,
    Cleric.standardClericAbilities,
    proficiencyBonus = 2,
    name = "Cleric"
  )

  val creatures = List(cleric,
                       Goblin.levelOneGoblin(),
                       Goblin.levelOneGoblin(),
                       Goblin.levelOneGoblin(),
                       Goblin.levelOneGoblin())

  val simulation = "Fighter vs Goblin"
  val (losses, wins) =
    SimulationRunner.run(BasicSimulation(creatures, LowestFirst), simulation, 1)

  logger.debug(s"$simulation simulation started")
  println(s"$wins Wins and $losses Losses")

  val data  = Seq("wins" -> wins, "losses" -> losses)
  val chart = BarChart(data)
  chart.show(title = simulation)
}
