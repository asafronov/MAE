SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

CREATE SCHEMA IF NOT EXISTS `MAStatistic_DB` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci ;
USE `MAStatistic_DB` ;

-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`GlobalSettings`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`GlobalSettings` (
  `id` INT NOT NULL AUTO_INCREMENT ,
<<<<<<< HEAD
  `systemtype` ENUM('mono', 'multi') NULL ,
=======
  `system_type` ENUM('mono', 'multi') NULL ,
>>>>>>> DB files added
  `days_count` INT NULL ,
  `sessions_count` INT NULL ,
  `agents_count` INT NULL ,
  `goods_count` INT NULL ,
<<<<<<< HEAD
  `productfunc_num` INT NULL ,
  `startprice` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`Agents`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Agents` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `consumption_good` VARCHAR(45) NULL ,
  `production_good` VARCHAR(45) NULL ,
  `create_count` DOUBLE NULL ,
  `stock` DOUBLE NULL ,
  `consumption_rate` DOUBLE NULL ,
  `alpha` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
=======
  `prodfunc_num` INT NULL ,
  `start_price_min` DOUBLE NULL ,
  `start_price_max` DOUBLE NULL ,
  `lambda_min` DOUBLE NULL ,
  `lambda_max` DOUBLE NULL ,
  `norma_min` DOUBLE NULL ,
  `norma_max` DOUBLE NULL ,
  `func_coef_min` DOUBLE NULL ,
  `func_coef_max` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;

-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`Simulations`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Simulations` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `settings_id` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  FOREIGN KEY (`settings_id`)  
)
ENGINE = InnoDB;

-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`DayHistory`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`DayHistory` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `simulation_id` INT NOT NULL ,
  `sim_day` INT NULL ,
  `production_sum` DOUBLE NULL ,
  `consumption_sum` DOUBLE NULL ,
  `relation_1010` DOUBLE NULL ,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`simulation_id`)  )
>>>>>>> DB files added
ENGINE = InnoDB;


-- -----------------------------------------------------
<<<<<<< HEAD
-- Table `MAStatistic_DB`.`Goods`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Goods` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(45) NULL ,
  `price` DOUBLE NULL ,
=======
-- Table `MAStatistic_DB`.`AgentsHeap`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`AgentsHeap` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `production_good` INT NULL ,
  `consumption_good` INT NULL ,
  `prod_coef` DOUBLE NULL ,
  `norma` DOUBLE NULL ,
  `lambda` DOUBLE NULL ,
>>>>>>> DB files added
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
<<<<<<< HEAD
-- Table `MAStatistic_DB`.`AgentCompGoods`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`AgentCompGoods` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `agent_id` INT NOT NULL ,
  `compgood_id` INT NOT NULL COMMENT 'Хранятся соответствия какие комплектющие нужны для каждого агента (товара производства)' ,
  `ratio` DOUBLE NULL COMMENT 'коэффициент производственной функции' ,
  PRIMARY KEY (`id`) )
=======
-- Table `MAStatistic_DB`.`Agents`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Agents` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `global_id` INT NOT NULL ,
  `simulation_id` INT NOT NULL ,
  PRIMARY KEY (`id`) ,
  FOREIGN KEY (`global_id`) ,
  FOREIGN KEY (`simulation_id`)
  )
>>>>>>> DB files added
ENGINE = InnoDB;


-- -----------------------------------------------------
<<<<<<< HEAD
-- Table `MAStatistic_DB`.`History`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`History` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `production_sum` DOUBLE NULL ,
  `consumption_sum` DOUBLE NULL ,
  `10_10` DOUBLE NULL ,
  `uncertainty` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
=======
-- Table `MAStatistic_DB`.`Goods`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Goods` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `global_id` INT NOT NULL ,
  `settings_id` INT NOT NULL ,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`global_id`) ,
  FOREIGN KEY (`settings_id`)  )
>>>>>>> DB files added
ENGINE = InnoDB;


-- -----------------------------------------------------
<<<<<<< HEAD
-- Table `MAStatistic_DB`.`ProductionHistory`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`ProductionHistory` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `day` INT NULL ,
  `good_id` INT NULL ,
  `production_count` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`ConsumtionHistory`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`ConsumtionHistory` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `day` INT NULL ,
  `good_id` INT NULL ,
  `consumtion_count` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
=======
-- Table `MAStatistic_DB`.`Markets`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`Markets` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `global_id` INT NOT NULL ,
  `settings_id` INT NOT NULL ,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`global_id`) ,
  FOREIGN KEY (`settings_id`)  )
ENGINE = InnoDB;

-- -----------------------------------------------------
-- Table `MAStatistic_DB`.`AllGoods`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`AllGoods` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(10),
  PRIMARY KEY (`id`)  )
>>>>>>> DB files added
ENGINE = InnoDB;


-- -----------------------------------------------------
<<<<<<< HEAD
-- Table `MAStatistic_DB`.`PriceHistory`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`PriceHistory` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `day` INT NULL ,
  `session` INT NULL ,
  `good_id` INT NULL ,
  `price` DOUBLE NULL ,
  PRIMARY KEY (`id`) )
ENGINE = InnoDB;

=======
-- Table `MAStatistic_DB`.`AllMarkets`
-- -----------------------------------------------------
CREATE  TABLE IF NOT EXISTS `MAStatistic_DB`.`AllMarkets` (
  `id` INT NOT NULL AUTO_INCREMENT ,
  `name` CHAR(7),
  `good1` INT NOT NULL ,
  `good2` INT NOT NULL ,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`good1`) ,
  FOREIGN KEY (`good2`)  )
ENGINE = InnoDB;



>>>>>>> DB files added
USE `MAStatistic_DB` ;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
