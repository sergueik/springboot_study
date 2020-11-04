CREATE DATABASE IF NOT EXISTS cardb;
-- intend to be equivalent but bad syntax
-- IF NOT EXISTS(  SELECT SCHEMA_NAME   FROM INFORMATION_SCHEMA.SCHEMATA  WHERE SCHEMA_NAME = 'cardb' )
--  BEGIN
--    CREATE DATABASE cardb 
---  END
-- GO

-- error in h2:
--- org.h2.jdbc.JdbcSQLException: Syntax error in SQL statement "CREATE DATABASE[*] CARDB "; 
USE cardb;

DROP TABLE IF EXISTS carinfo;

CREATE TABLE carinfo (
   id INT NOT NULL PRIMARY KEY AUTO_INCREMENT,
   yearofmanufacture INT NOT NULL,
   model VARCHAR(64) NOT NULL,
   make VARCHAR(64) NOT NULL,
   suggestedretailprice FLOAT NOT NULL,
   fullprice FLOAT NOT NULL,
   rebateamount FLOAT NOT NULL,
   createdate DATETIME NOT NULL,
   updatedate DATETIME NOT NULL
);
