DROP SCHEMA IF EXISTS `geoclimate` ;
CREATE SCHEMA IF NOT EXISTS `geoclimate` DEFAULT CHARACTER SET utf8 COLLATE latin1_swedish_ci ;
USE `geoclimate` ;

-- -----------------------------------------------------
-- Table `geoclimate`.`datasets`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `geoclimate`.`datasets` ;

CREATE  TABLE IF NOT EXISTS `geoclimate`.`datasets` (
  `dataset_id` INT(11) NOT NULL AUTO_INCREMENT ,
  `dataset_name` VARCHAR(45) NOT NULL ,
  `table_name` VARCHAR(20) NOT NULL ,
  `maskset_id` INT NOT NULL ,
  `xmin` DECIMAL(8,4) NOT NULL ,
  `xmax` DECIMAL(8,4) NOT NULL ,
  `ymin` DECIMAL(8,4) NOT NULL ,
  `ymax` DECIMAL(8,4) NOT NULL ,
  `xres` DECIMAL(8,4) NOT NULL ,
  `yres` DECIMAL(8,4) NOT NULL ,
  `source` VARCHAR(100) NOT NULL ,
  `remarks` TEXT NULL DEFAULT NULL ,
  PRIMARY KEY (`dataset_id`) )
ENGINE = MyISAM
AUTO_INCREMENT = 2
DEFAULT CHARACTER SET = utf8;


-- -----------------------------------------------------
-- Table `geoclimate`.`masksets`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `geoclimate`.`masksets` ;

CREATE  TABLE IF NOT EXISTS `geoclimate`.`masksets` (
  `maskset_id` INT NOT NULL ,
  `maskset_name` VARCHAR(15) NOT NULL ,
  `maskset_desc` TEXT NULL ,
  `xmin` FLOAT NOT NULL ,
  `xmax` FLOAT NOT NULL ,
  `xres` FLOAT NOT NULL ,
  `ymin` FLOAT NOT NULL ,
  `ymax` FLOAT NOT NULL ,
  `yres` FLOAT NOT NULL ,
  PRIMARY KEY (`maskset_id`) )
ENGINE = MyISAM;


-- -----------------------------------------------------
-- Table `geoclimate`.`maskcells`
-- -----------------------------------------------------
DROP TABLE IF EXISTS `geoclimate`.`maskcells` ;

CREATE  TABLE IF NOT EXISTS `geoclimate`.`maskcells` (
  `maskset_id` INT NOT NULL ,
  `cell` INT NOT NULL ,
  `iso3` CHAR(3) NULL ,
  `land` TINYINT(1)  NOT NULL DEFAULT FALSE ,
  `arable` TINYINT(1)  NOT NULL DEFAULT FALSE )
ENGINE = MyISAM;

CREATE INDEX `mc_idx` ON `geoclimate`.`maskcells` (`maskset_id` ASC, `cell` ASC) ;
