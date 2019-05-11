package io.github.tjheslin1.dmspredictor

import cats.implicits._
import eu.timepit.refined.api.Refined.unsafeApply
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import io.github.tjheslin1.dmspredictor.classes.Player
import io.github.tjheslin1.dmspredictor.classes.barbarian.{Barbarian, BaseBarbarian, Berserker}
import io.github.tjheslin1.dmspredictor.classes.cleric.{BaseCleric, Cleric}
import io.github.tjheslin1.dmspredictor.classes.fighter._
import io.github.tjheslin1.dmspredictor.classes.rogue.{BaseRogue, Rogue}
import io.github.tjheslin1.dmspredictor.classes.wizard._
import io.github.tjheslin1.dmspredictor.equipment.Equipment
import io.github.tjheslin1.dmspredictor.equipment.armour._
import io.github.tjheslin1.dmspredictor.equipment.weapons._
import io.github.tjheslin1.dmspredictor.model._
import io.github.tjheslin1.dmspredictor.monsters._
import io.github.tjheslin1.dmspredictor.monsters.vampire.Vampire

trait ArgParser {

  implicit val rollStrategy: RollStrategy

  implicit val simulationConfigDecoder: Decoder[SimulationConfig] = deriveDecoder[SimulationConfig]

  implicit val barbarianDecoder: Decoder[Barbarian] = new Decoder[Barbarian] {
    def apply(c: HCursor): Result[Barbarian] =
      for {
        levelInt      <- c.downField("level").as[Int]
        level         = Level(levelInt)
        statsStr      <- c.downField("stats").as[String]
        stats         <- baseStatsConverter(c, statsStr)
        weapon        <- c.downField("weapon").as[String]
        armour        <- c.downField("armour").as[String]
        offHand       <- c.downField("offHand").as[String]
        skillsStr     <- c.downField("skills").as[String]
        skills        <- skillsConverter(c, skillsStr)
        barbarianName <- c.downField("name").as[String]
      } yield {
        val health = BaseBarbarian.calculateHealth(level, stats.constitution)
        Barbarian(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          BaseBarbarian.rageUsagesPerLevel(level),
          skills,
          armourLookup(armour.toLowerCase),
          offHandLookup.get(offHand.toLowerCase),
          proficiencyBonus = ProficiencyBonus.fromLevel(level),
          name = barbarianName
        )
      }
  }

  implicit val berserkerDecoder: Decoder[Berserker] = new Decoder[Berserker] {
    def apply(c: HCursor): Result[Berserker] =
      for {
        levelInt      <- c.downField("level").as[Int]
        level         = Level(levelInt)
        statsStr      <- c.downField("stats").as[String]
        stats         <- baseStatsConverter(c, statsStr)
        weapon        <- c.downField("weapon").as[String]
        armour        <- c.downField("armour").as[String]
        offHand       <- c.downField("offHand").as[String]
        skillsStr     <- c.downField("skills").as[String]
        skills        <- skillsConverter(c, skillsStr)
        berserkerName <- c.downField("name").as[String]
      } yield {
        val health = BaseBarbarian.calculateHealth(level, stats.constitution)
        Berserker(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          BaseBarbarian.rageUsagesPerLevel(level),
          skills,
          armourLookup(armour.toLowerCase),
          offHandLookup.get(offHand.toLowerCase),
          proficiencyBonus = ProficiencyBonus.fromLevel(level),
          name = berserkerName
        )
      }
  }

  implicit val clericDecoder: Decoder[Cleric] = new Decoder[Cleric] {
    def apply(c: HCursor): Result[Cleric] =
      for {
        levelInt   <- c.downField("level").as[Int]
        level      = Level(levelInt)
        statsStr   <- c.downField("stats").as[String]
        stats      <- baseStatsConverter(c, statsStr)
        weapon     <- c.downField("weapon").as[String]
        armour     <- c.downField("armour").as[String]
        offHand    <- c.downField("offHand").as[String]
        skillsStr  <- c.downField("skills").as[String]
        skills     <- skillsConverter(c, skillsStr)
        clericName <- c.downField("name").as[String]
      } yield {
        val health = BaseCleric.calculateHealth(level, stats.constitution)
        Cleric(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          skills,
          Cleric.clericSpellSlots(level),
          Cleric.standardClericSpellList,
          armour = armourLookup(armour.toLowerCase),
          offHand = offHandLookup.get(offHand.toLowerCase),
          name = clericName
        )
      }
  }

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

  implicit val championDecoder: Decoder[Champion] = new Decoder[Champion] {
    def apply(c: HCursor): Result[Champion] =
      for {
        levelInt     <- c.downField("level").as[Int]
        level        = Level(levelInt)
        statsStr     <- c.downField("stats").as[String]
        stats        <- baseStatsConverter(c, statsStr)
        weapon       <- c.downField("weapon").as[String]
        armour       <- c.downField("armour").as[String]
        offHand      <- c.downField("offHand").as[String]
        skillsStr    <- c.downField("skills").as[String]
        skills       <- skillsConverter(c, skillsStr)
        style        <- c.downField("fightingStyles").as[String]
        championName <- c.downField("name").as[String]
      } yield {
        val health = BaseFighter.calculateHealth(level, stats.constitution)
        Champion(
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
          name = championName
        )
      }
  }

  implicit val rogueDecoder: Decoder[Rogue] = new Decoder[Rogue] {
    override def apply(c: HCursor): Result[Rogue] =
      for {
        levelInt  <- c.downField("level").as[Int]
        level     = Level(levelInt)
        statsStr  <- c.downField("stats").as[String]
        stats     <- baseStatsConverter(c, statsStr)
        weapon    <- c.downField("weapon").as[String]
        armour    <- c.downField("armour").as[String]
        offHand   <- c.downField("offHand").as[String]
        skillsStr <- c.downField("skills").as[String]
        skills    <- skillsConverter(c, skillsStr)
        rogueName <- c.downField("name").as[String]
      } yield {
        val health = BaseRogue.calculateHealth(level, stats.constitution)
        Rogue(
          level,
          health,
          health,
          stats,
          weaponsLookup(weapon.toLowerCase),
          skills,
          armourLookup(armour.toLowerCase),
          offHandLookup.get(offHand.toLowerCase),
          proficiencyBonus = ProficiencyBonus.fromLevel(level),
          name = rogueName
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
    def apply(c: HCursor): Result[Goblin] =
      for {
        goblinName <- c.downField("name").as[String]
      } yield {
        val health = Goblin.calculateHealth
        Goblin(health, health, name = goblinName)
      }
  }

  implicit val werewolfDecoder: Decoder[Werewolf] = new Decoder[Werewolf] {
    def apply(c: HCursor): Result[Werewolf] =
      for {
        werewolfName <- c.downField("name").as[String]
      } yield {
        val health = Werewolf.calculateHealth
        Werewolf(
          health,
          health,
          name = werewolfName
        )
      }
  }

  implicit val vampireDecoder: Decoder[Vampire] = new Decoder[Vampire] {
    def apply(c: HCursor): Result[Vampire] =
      for {
        vampireName <- c.downField("name").as[String]
      } yield {
        val health = Vampire.calculateHealth
        Vampire(
          health,
          health,
          name = vampireName
        )
      }
  }

  implicit val zombieDecoder: Decoder[Zombie] = new Decoder[Zombie] {
    def apply(c: HCursor): Result[Zombie] =
      for {
        zombieName <- c.downField("name").as[String]
      } yield {
        val health = Zombie.calculateHealth
        Zombie(
          health,
          health,
          name = zombieName
        )
      }
  }

  implicit val playerDecoder: Decoder[Player] = new Decoder[Player] {
    def apply(c: HCursor): Result[Player] =
      for {
        playerClass <- c.downField("class").as[String]
        decoderOpt  = playerClassDecoderLookup.get(playerClass.toLowerCase)
        decoder <- decoderOpt.fold[Result[Decoder[_]]](
                    DecodingFailure(s"Unknown player class: $playerClass", c.history).asLeft)(d =>
                    d.asRight)
        result <- decoder(c).asInstanceOf[Result[Player]]
      } yield result
  }

  implicit val monsterDecoder: Decoder[Monster] = new Decoder[Monster] {
    def apply(c: HCursor): Result[Monster] =
      for {
        monster    <- c.downField("monster").as[String]
        decoderOpt = monsterDecoderLookup.get(monster.toLowerCase)
        decoder <- decoderOpt.fold[Result[Decoder[_]]](
                    DecodingFailure(s"Unknown monster: $monster", c.history).asLeft)(d => d.asRight)
        result <- decoder(c).asInstanceOf[Result[Monster]]
      } yield result
  }

  val playerClassDecoderLookup: Map[String, Decoder[_]] = Map(
    "barbarian" -> Decoder[Barbarian],
    "berserker" -> Decoder[Berserker],
    "cleric"    -> Decoder[Cleric],
    "fighter"   -> Decoder[Fighter],
    "champion"  -> Decoder[Champion],
    "rogue"     -> Decoder[Rogue],
    "wizard"    -> Decoder[Wizard]
  )

  val monsterDecoderLookup: Map[String, Decoder[_]] = Map(
    "goblin"   -> Decoder[Goblin],
    "werewolf" -> Decoder[Werewolf],
    "vampire"  -> Decoder[Vampire],
    "zombie"   -> Decoder[Zombie]
  )

  val weaponsLookup: Map[String, Weapon] = Map(
    Shortsword.name.toLowerCase -> Shortsword,
    Greataxe.name.toLowerCase   -> Greataxe,
    Greatsword.name.toLowerCase -> Greatsword
  )

  val armourLookup: Map[String, Armour] = Map(
    "none"                      -> NoArmour,
    NoArmour.name.toLowerCase   -> NoArmour,
    ChainShirt.name.toLowerCase -> ChainShirt
  )

  val offHandLookup: Map[String, Equipment] = Map(
    Shield.name.toLowerCase     -> Shield,
    Shortsword.name.toLowerCase -> Shortsword
  )

  val fightingStyleLookup: Map[String, FighterFightingStyle] = Map(
    "archery"             -> Archery,
    "defense"             -> Defense,
    "dueling"             -> Dueling,
    "greatweaponfighting" -> GreatWeaponFighting,
    "protection"          -> Protection,
    "twoweaponfighting"   -> TwoWeaponFighting
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
