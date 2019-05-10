package io.github.tjheslin1.dmspredictor

import cats.implicits._
import eu.timepit.refined.api.Refined.unsafeApply
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.wizard._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.equipment.weapons._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters._

trait ArgParser {

  implicit val rollStrategy: RollStrategy

  implicit val simulationConfigDecoder: Decoder[SimulationConfig] = deriveDecoder[SimulationConfig]

  implicit val fighterDecoder: Decoder[Fighter] = new Decoder[Fighter] {
    def apply(c: HCursor): Result[Fighter] =
      for {
        levelInt    <- c.downField("level").as[Int]
        level       = Level(levelInt)
        statsStr    <- c.downField("stats").as[String]
        stats       <- baseStatsConverter(c, statsStr)
        weapon      <- c.downField("weapon").as[String]
        armour      <- c.downField("armour").as[String]
        offHand     <- c.downField("offHand").as[String]
        skillsStr   <- c.downField("skills").as[String]
        skills      <- skillsConverter(c, skillsStr)
        style       <- c.downField("fightingStyles").as[String]
        fighterName <- c.downField("name").as[String]
      } yield {
        val health = BaseFighter.calculateHealth(level, stats.constitution)
        Fighter(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          skills,
          armourLookup(armour.toLowerCase),
          offHandLookup.get(offHand.toLowerCase),
          List(fightingStyleLookup(style.toLowerCase)),
          proficiencyBonus = ProficiencyBonus.fromLevel(level),
          name = fighterName
        )
      }
  }

  implicit val wizardDecoder: Decoder[Wizard] = new Decoder[Wizard] {
    def apply(c: HCursor): Result[Wizard] =
      for {
        levelInt   <- c.downField("level").as[Int]
        level      = Level(levelInt)
        statsStr   <- c.downField("stats").as[String]
        stats      <- baseStatsConverter(c, statsStr)
        weapon     <- c.downField("weapon").as[String]
        skillsStr  <- c.downField("skills").as[String]
        skills     <- skillsConverter(c, skillsStr)
        wizardName <- c.downField("name").as[String]
      } yield {
        val health = BaseWizard.calculateHealth(level, stats.constitution)
        Wizard(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          skills,
          Wizard.wizardSpellSlots(level),
          Wizard.standardWizardSpellList,
          proficiencyBonus = ProficiencyBonus.fromLevel(level),
          name = wizardName
        )
      }
  }

  implicit val goblinDecoder: Decoder[Goblin] = new Decoder[Goblin] {
    override def apply(c: HCursor): Result[Goblin] =
      for {
        goblinName <- c.downField("name").as[String]
      } yield {
        val health = Goblin.calculateHealth
        Goblin(health, health, name = goblinName)
      }
  }

  implicit val playerDecoder: Decoder[Player] = new Decoder[Player] {
    override def apply(c: HCursor): Result[Player] =
      for {
        playerClass <- c.downField("class").as[String]
        decoderOpt  = playerClassDecoderLookup.get(playerClass.toLowerCase)
        decoder <- decoderOpt.fold[Result[Decoder[_]]](
                    DecodingFailure(s"Unknown player class $playerClass", c.history).asLeft)(d =>
                    d.asRight)
        result <- decoder(c).asInstanceOf[Result[Player]]
      } yield result
  }
//    List[Decoder[Player]](Decoder[Wizard].widen).reduceLeft(_ or _)

  val playerClassDecoderLookup: Map[String, Decoder[_]] = Map(
    "fighter" -> Decoder[Fighter],
    "wizard"  -> Decoder[Wizard]
  )

  implicit val monsterDecoder: Decoder[Monster] =
    List[Decoder[Monster]](Decoder[Goblin].widen).reduceLeft(_ or _)

  val weaponsLookup: Map[String, Weapon] = Map(
    Shortsword.name.toLowerCase -> Shortsword,
    Greatsword.name.toLowerCase -> Greatsword
  )

  val armourLookup: Map[String, Armour] = Map(
    NoArmour.name.toLowerCase   -> NoArmour,
    ChainShirt.name.toLowerCase -> ChainShirt
  )

  val offHandLookup: Map[String, Equipment] = Map(
    Shield.name.toLowerCase     -> Shield,
    Shortsword.name.toLowerCase -> Shortsword
  )

  val fightingStyleLookup: Map[String, FighterFightingStyle] = Map(
    "archery" -> Archery,
    "defense" -> Defense,
    "dueling" -> Dueling,
    "greatweaponfighting" -> GreatWeaponFighting,
    "protection" -> Protection,
    "twoweaponfighting" -> TwoWeaponFighting
  )

  def baseStatsConverter(c: HCursor, statsCsv: String): Result[BaseStats] =
    for {
      ints <- Either
               .catchNonFatal(statsCsv.split(",").map(_.toInt))
               .leftMap(e => DecodingFailure(e.getMessage, c.history))
      baseStats <- ints match {
                    case Array(str, dex, con, wis, int, cha) =>
                      BaseStats(unsafeApply(str),
                                unsafeApply(dex),
                                unsafeApply(con),
                                unsafeApply(wis),
                                unsafeApply(int),
                                unsafeApply(cha)).asRight
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
