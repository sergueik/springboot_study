

-- SQLite
DROP TABLE IF EXISTS "hosts";
CREATE TABLE "hosts" ( `id` INTEGER, `hostname` TEXT NOT NULL, `appid` TEXT, `environment` TEXT, `datacenter` TEXT, `addtime` TEXT, PRIMARY KEY(`id`) )

DROP TABLE IF EXISTS "application";
CREATE TABLE application( aid bigint primary key , aname NVARCHAR(10) not null );

DROP TABLE IF EXISTS "axixs";
CREATE TABLE axixs( sid bigint, iid bigint, aid bigint )

DROP TABLE IF EXISTS "server";
CREATE TABLE server( sid bigint primary key , sname NVARCHAR(10) not null );

DROP TABLE IF EXISTS "instance";
CREATE TABLE instance( iid bigint primary key , iname NVARCHAR(10) not null )

DROP TABLE IF EXISTS "item";
CREATE TABLE item( iid bigint primary key , iname NVARCHAR(10) not null, iprice bigint, cid bigint, FOREIGN KEY(cid) REFERENCES customer(cid) )

-- MYSQL (sans timestamp)
DROP TABLE IF EXISTS `hosts`;

CREATE TABLE `hosts` ( `id` INT AUTO_INCREMENT, `hostname` VARCHAR(40) NOT NULL, `appid`  VARCHAR(40), `environment`  VARCHAR(40), `datacenter`  VARCHAR(40), PRIMARY KEY(`id`) );

DROP TABLE IF EXISTS `application`;
CREATE TABLE application( aid INT AUTO_INCREMENT, aname VARCHAR(20) not null, PRIMARY KEY(`aid`) );

DROP TABLE IF EXISTS `server`;
CREATE TABLE server( sid INT AUTO_INCREMENT, sname VARCHAR(10) not null, PRIMARY KEY(`sid`) );

DROP TABLE IF EXISTS `axixs`;
CREATE TABLE axixs( sid int, iid int, aid int );

DROP TABLE IF EXISTS `customer`;
CREATE TABLE customer( cid INT AUTO_INCREMENT, cname VARCHAR(30) not null, PRIMARY KEY(`cid`)  );

DROP TABLE IF EXISTS `item`;
CREATE TABLE item( iid INT AUTO_INCREMENT, iname VARCHAR(10) not null, iprice int, cid int, PRIMARY KEY(`iid`), FOREIGN KEY(`cid`) REFERENCES customer(`cid`) );

DROP TABLE IF EXISTS `instance`;
CREATE TABLE instance( iid INT AUTO_INCREMENT, iname VARCHAR(10) not null, PRIMARY KEY(`iid`));


insert into instance (iid,iname) values (1001,'instance01'), (1002,'instance02'), (1003,'instance03'), (1004,'instance04'), (1005,'instance05');
insert into application (aid,aname) values ( 10001,'application01'), (10002,'application02');
insert into server (sid,sname) values (101,'hostname00'), (102,'hostname01'), (103,'hostname02'), (105,'hostname05');


-- REGEXP 
select * from students where email REGEXP '(test4@gmail.com|test2@gmail.com)';
insert into students (name, email, phone_no) values  ('peter','test2@gmail.com',12345),('john', 'test4@gmail.com',12345);

-- SQL
exec sys.sp_readerrorlog 0, 1, 'listening';

create database cluster;
use cluster;

create table hosts(
id      bigint primary key identity(1,1),
hostname    NVARCHAR(30) not null,
app	NVARCHAR(30) not null,
environmnt	NVARCHAR(30) not null,
-- TODO: rename to "datacenter"
datacenter	NVARCHAR(30) not null,
addtime datetime not null default current_timestamp);

-- Insert Sample data
insert into hosts(hostname,appid,environment,datacenter) values('hostname10','redis','qa','west');
insert into hosts(hostname,appid,environment,datacenter) values('hostname11','redis','prod','east');
insert into hosts(hostname,appid,environment,datacenter) values('hostname12','mongo','qa','west');

-- overlap with YAML cluster.yml
insert into hosts(hostname,appid,environment,datacenter) values('hostname03','redis','prod','east');