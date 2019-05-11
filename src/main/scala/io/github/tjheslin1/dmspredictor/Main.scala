package io.github.tjheslin1.dmspredictor

import cats.data.NonEmptyList
import cats.syntax.either._
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.parser.decode
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters.Monster
import io.github.tjheslin1.dmspredictor.simulation.{BasicSimulation, SimulationRunner}
import io.github.tjheslin1.dmspredictor.strategy._

/*
sbt 'run "{
 "simulationName": "Wizard vs Goblin",
 "simulations": 2,
 "focus": "LowestFirst",
 "players": [
   {
    "class": "wizard",
    "level": 4,
    "stats": "10,10,14,14,14,10",
    "weapon": "Shortsword",
    "skills": "1,1",
    "name": "TestWizard"
   }
 ],
 "monsters": [
  {
    "monster": "goblin",
    "name": "TestGoblin"
  }
 ]
}"'

// Wizard and Fighter vs Goblins
sbt 'run "{\"simulationName\":\"Wizard and Fighter vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\":\"wizard\",\"level\":5,\"stats\":\"10,10,14,14,14,10\",\"weapon\":\"Shortsword\",\"skills\":\"1,1\",\"name\":\"TestWizard\"},{\"class\": \"fighter\",\"level\": 4,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Greatsword\",\"armour\": \"ChainShirt\",\"offHand\": \"none\",\"skills\": \"1,1\",\"fightingStyles\": \"GreatWeaponFighting\",\"name\": \"TestFighter\"}],\"monsters\":[{\"monster\": "goblin", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Fighter vs Goblins
sbt 'run "{\"simulationName\":\"Fighter vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"fighter\",\"level\": 5,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Greatsword\",\"armour\": \"ChainShirt\",\"offHand\": \"none\",\"skills\": \"1,1\",\"fightingStyles\": \"Defense\",\"name\": \"TestFighter\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Champion vs Goblins
sbt 'run "{\"simulationName\":\"Champion vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"champion\",\"level\": 4,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Shortsword\",\"armour\": \"ChainShirt\",\"offHand\": \"Shortsword\",\"skills\": \"1,1\",\"fightingStyles\": \"TwoWeaponFighting\",\"name\": \"TestChampion\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Barbarian vs Goblins
sbt 'run "{\"simulationName\":\"Barbarian vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"barbarian\",\"level\": 4,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Greataxe\",\"armour\": \"none\",\"offHand\": \"none\",\"skills\": \"1,1\",\"name\": \"TestBarbarian\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Berserker vs Goblins
sbt 'run "{\"simulationName\":\"Berserker vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"berserker\",\"level\": 5,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Greataxe\",\"armour\": \"none\",\"offHand\": \"none\",\"skills\": \"1,1\",\"name\": \"TestBerserker\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Cleric vs Goblins
sbt 'run "{\"simulationName\":\"Cleric vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"cleric\",\"level\": 5,\"stats\": \"10,10,10,14,14,14\",\"weapon\": \"Shortsword\",\"armour\": \"Chainshirt\",\"offHand\": \"Shield\",\"skills\": \"1,1\",\"name\": \"TestCleric\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Rogue vs Goblins
sbt 'run "{\"simulationName\":\"Rogue vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"rogue\",\"level\": 5,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Shortsword\",\"armour\": \"none\",\"offHand\": \"none\",\"skills\": \"1,1\",\"name\": \"TestRogue\"}],\"monsters\":[{\"monster\": \"goblin\", \"name\":\"TestGoblin\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin2\"},{\"monster\": \"goblin\", \"name\":\"TestGoblin3\"}]}"'

// Rogue vs Zombie, Vampire, Werewolf
sbt 'run "{\"simulationName\":\"Rogue vs Goblins\",\"simulations\":2,\"focus\":\"LowestFirst\",\"players\":[{\"class\": \"rogue\",\"level\": 5,\"stats\": \"14,14,14,10,10,10\",\"weapon\": \"Shortsword\",\"armour\": \"none\",\"offHand\": \"none\",\"skills\": \"1,1\",\"name\": \"TestRogue\"}],\"monsters\":[{\"monster\": \"zombie\", \"name\":\"TestZombie\"},{\"monster\": \"vampire\", \"name\":\"TestVampire\"},{\"monster\": \"werewolf\", \"name\":\"TestWerewolf\"}]}"'
 */

case class SimulationConfig(simulationName: String,
                            simulations: Int,
                            focus: String,
                            players: NonEmptyList[Player],
                            monsters: NonEmptyList[Monster])

object Main extends App with ArgParser with scalax.chart.module.Charting with LazyLogging {

  implicit val rollStrategy = Dice.defaultRandomiser

  val x: Either[Error, (SimulationConfig, BasicSimulation)] = for {
    config      <- decode[SimulationConfig](args(0))
    parsedFocus <- parseFocus(config.focus)
  } yield (config, BasicSimulation(config.players.toList ++ config.monsters.toList, parsedFocus))

  x match {
    case Left(e) => throw new RuntimeException(s"Error parsing JSON\n${e.getMessage}", e)
    case Right((config, basicSimulation)) =>
      val (losses, wins) =
        SimulationRunner.run(basicSimulation, config.simulationName, config.simulations)

      logger.debug(s"${config.simulationName} simulation started")
      println(s"$wins Wins and $losses Losses")

      val data  = Seq("wins" -> wins, "losses" -> losses)
      val chart = BarChart(data)
      chart.show(title = config.simulationName)
  }

  def parseFocus(focus: String): Either[Error, Focus] = focus.toLowerCase match {
    case "lowestfirst" => LowestFirst.asRight
    case "randomfocus" => RandomFocus.asRight
    case _             => Left(ParsingFailure(s"unknown focus strategy provided: $focus", null))
  }
}
