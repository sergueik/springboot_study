-- drop database if exists example_docker_db;
-- create database example_docker_db;
use test;

--
-- Table structure for table `cache_table`
--

DROP TABLE IF EXISTS `cache_table`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `cache_table` (
  `id`        bigint(20)   NOT NULL,
  `ins_date`  datetime     NOT NULL,
  `fname`     varchar(255) NOT NULL,
  `ds`        varchar(255) NOT NULL,
  `comment`   varchar(255) DEFAULT NULL,
  INDEX(`FNAME`),
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `CACHE_TABLE`
--

INSERT INTO `cache_table` (id, ins_date, fname, ds)
VALUES
( 1, now(), 'fname-1', 'ds-1'), 
( 2, now(), 'fname-1', 'ds-2'), 
( 3, now(), 'fname-1', 'ds-3'), 
( 4, now(), 'fname-2', 'ds-4'), 
( 5, now(), 'fname-2', 'ds-5'), 
( 6, now(), 'fname-3', 'ds-5');

