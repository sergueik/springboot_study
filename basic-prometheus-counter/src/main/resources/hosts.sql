

-- SQLite
DROP TABLE IF EXISTS "hosts";
CREATE TABLE "hosts" ( `id` INTEGER, `hostname` TEXT NOT NULL, `appid` TEXT, `environment` TEXT, `datacenter` TEX, `addtime` TEXT, PRIMARY KEY(`id`) )

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

-- overlap with YAML cluster.yml
insert into hosts(hostname,appid,environment,datacenter) values('hostname03','redis','prod','east');