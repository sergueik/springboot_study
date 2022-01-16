CREATE DATABASE IF NOT EXISTS join_check;
USE join_check;
DROP TABLE IF EXISTS t1;
CREATE TABLE t1 ( id INT PRIMARY KEY, pattern VARCHAR(50) NOT NULL, name varchar(250) );
DROP TABLE IF EXISTS t2;
CREATE TABLE t2 ( id VARCHAR(50) PRIMARY KEY, pattern VARCHAR(50) NOT NULL, data varchar(250) );

INSERT INTO t1(id, pattern,name) VALUES(1,'Divot','Basant'), (2,'Brick','Santosh'), (3,'Grid','Chinmaya');
INSERT INTO t2(id, pattern,data) VALUES('A','Brick','B'), ('B','Grid','S'), ('C','Diamond','C');
