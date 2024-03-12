-- PostgreSQL
-- NOTE: comment when using MySQL - uncomment when using PostgreSQL
-- see also: https://stackoverflow.com/questions/9556474/automatically-populate-a-timestamp-field-in-postgresql-when-a-new-row-is-inserte
CREATE TABLE IF NOT EXISTS rest ( id serial PRIMARY KEY NOT NULL, key varchar(100) NOT NULL, value varchar(250) NOT NULL, timestamp timestamp default current_timestamp, rand smallint NOT NULL );
-- MySQL
-- NOTE: comment when using PostgreSQL -  uncomment when using MySQL
--- CREATE TABLE IF NOT EXISTS rest ( id int AUTO_INCREMENT, `key` varchar(100) NOT NULL, value varchar(250) NOT NULL, rand integer NOT NULL, PRIMARY KEY(`id`)  );


-- INSERT INTO rest (`key`,`value`, `rand`) values ('example 1', 'product 1',10),('example 2', 'product 2',10),('example 3', 'product 3',10),('example 4', 'product 4',10);
-- org.springframework.jdbc.datasource.init.UncategorizedScriptException: Failed
-- to execute database script from resource [URL [file:/C:/developer/sergueik/springboot_study/basic-postgresql/target/classes/schema.sql]]; nested exception is java.lang.IllegalArgumentException: 'script' must not be null or empty
