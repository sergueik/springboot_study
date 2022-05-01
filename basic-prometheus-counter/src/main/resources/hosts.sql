

-- SQLite
CREATE TABLE "hosts" ( `id` INTEGER, `hostname` TEXT NOT NULL, `app` TEXT, `environment` TEXT, `domain` TEX, `addtime` TEXT, PRIMARY KEY(`id`) )

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
domain	NVARCHAR(30) not null,
addtime datetime not null default current_timestamp);

insert into hosts(hostname,app,environment,domain) values('hostname00','redis','qa','west');
insert into hosts(hostname,app,environment,domain) values('hostname01','redis','prod','east');