package io.github.tjheslin1.dmspredictor

import cats.implicits._
import eu.timepit.refined.auto._
import io.circe.Decoder.Result
import io.circe._
import io.github.tjheslin1.dmspredictor.classes.wizard._
import io.github.tjheslin1.dmspredictor.equipment.weapons._
import io.github.tjheslin1.dmspredictor.model._

object ArgParser {

  // import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  /*
  {
    "level": 0,
    "stats": "10,10,10,10,10,10",
    "weapon": Shortsword,
    "skills": "2,2",
    "spellsKnown": "FireBolt,MagicMissile,AcidArrow,Fireball",
    "castShieldAsReaction": true,
    "mageArmourPrepared": true,
    "armour": NoArmour,
    "offHand": None,
    "name": "Tom"
  }
   */

  implicit val wizardDecoder: Decoder[Wizard] = new Decoder[Wizard] {
    def apply(c: HCursor): Result[Wizard] =
      for {
        levelInt <- c.downField("level").as[Int]
        level     = Level(levelInt)
        statsStr  <- c.downField("stats").as[String]
        stats     <- baseStatsConverter(c, statsStr)
        weapon    <- c.downField("weapon").as[String]
        skillsStr <- c.downField("skills").as[String]
        skills    <- skillsConverter(c, skillsStr)
      wizardName <- c.downField("name").as[String]
      } yield {
        val health = BaseWizard.calculateHealth(level, stats.constitution)
        Wizard(level,
               health,
               health,
               stats,
               weaponsLookup(weapon),
               skills,
               Wizard.wizardSpellSlots(level),
               Wizard.standardWizardSpellList,
          name = wizardName
        )
      }
  }

  val weaponsLookup: Map[String, Weapon] = Map(
    Shortsword.name -> Shortsword,
    Greatsword.name -> Greatsword
  )

  def baseStatsConverter(c: HCursor, statsCsv: String): Result[BaseStats] =
    for {
      ints <- Either
               .catchNonFatal(statsCsv.split(",").map(_.toInt))
               .leftMap(e => DecodingFailure(e.getMessage, c.history))
      baseStats <- ints match {
                    case Array(str, dex, con, wis, int, cha) =>
                      BaseStats(str, dex, con, wis, int, cha).asRight
                    case _ =>
                      DecodingFailure(s"$statsCsv did not match expected format for stats",
                                      c.history).asLeft
                  }
    } yield baseStats

  def skillsConverter(c: HCursor, skillsCsv: String): Result[Skills] =
    for {
      ints <- Either
               .catchNonFatal(skillsCsv.split(",").map(_.toInt))
               .leftMap(e => DecodingFailure(e.getMessage, c.history))
      skills <- ints match {
                 case Array(perception, stealth) => Skills(perception, stealth).asRight
                 case _ =>
                   DecodingFailure(s"$skillsCsv did not match expected format for skills",
                                   c.history).asLeft
               }
    } yield skills
}
