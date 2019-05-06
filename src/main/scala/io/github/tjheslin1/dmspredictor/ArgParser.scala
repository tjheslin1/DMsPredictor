package io.github.tjheslin1.dmspredictor

import eu.timepit.refined.auto._
import io.circe.Decoder.Result
import io.circe._
import io.github.tjheslin1.dmspredictor.classes.wizard.{BaseWizard, Wizard}
import io.github.tjheslin1.dmspredictor.model.BaseStats

object ArgParser {

  // import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  /*
  {
    "level": 0,
    "stats": "10,10,10,10,10,10",
    "baseWeapon": Shortsword,
    "skills": "2,2",
    "spellSlots": "4,3,3",
    "spellsKnown": "FireBolt,MagicMissile,AcidArrow,Fireball",
    "castShieldAsReaction": true,
    "mageArmourPrepared": true,
    "armour": NoArmour,
    "offHand": None,
    "name": "Tom"
  }
   */

  import cats.implicits._

  def baseStatsConverter(c: HCursor, statsCsv: String): Result[BaseStats] =
    for {
      ints <- Either.catchNonFatal(statsCsv.split(",").map(_.toInt)).leftMap(e => DecodingFailure(e.getMessage, c.history))
      baseStats <- ints match {
              case Array(str, dex, con, wis, int, cha) => BaseStats(str, dex, con, wis, int, cha).asRight
              case _ => DecodingFailure(s"$statsCsv did not match expected format for stats", c.history).asLeft
            }
    } yield baseStats

  implicit val wizardDecoder: Decoder[Wizard] = new Decoder[Wizard] {
    def apply(c: HCursor): Result[Wizard] =
      for {
        level <- c.downField("level").as[Int]
        stats <- baseStatsConverter(c.downField("stats"))
      } yield {
        val health = BaseWizard.calculateHealth(level, stats.constitution)
        Wizard(level, )
      }
  }
}
