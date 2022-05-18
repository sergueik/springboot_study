-- Loose SQL Schema

use test
create table instance(
  iid bigint primary key ,
  iname NVARCHAR(10) not null
);


create table server(
  sid bigint primary key ,
  sname NVARCHAR(10) not null
);


create table application(
  aid bigint primary key ,
  aname NVARCHAR(10) not null
);


create table axixs(
  sid bigint,
  iid bigint,
  aid bigint
);

-- NOTE: not using 
-- CONSTRAINT fk_sid FOREIGN KEY (sid) REFERENCES server(sid)



delete from server;
insert into server(sname,sid) values('hostname00',101);
insert into server(sname,sid) values('hostname01',102);
insert into server(sname,sid) values('hostname02',103);

insert into application(aname,aid) values('application01',10001);
delete from application where aid = 10002;
insert into application(aname,aid) values('application02',10002);



insert into instance(iname,iid) values('instance01',1001);
insert into instance(iname,iid) values('instance02',1002);

insert into instance(iname,iid) values('instance03',1003);

insert into instance(iname,iid) values('instance04',1004);

insert into instance(iname,iid) values('instance05',1005);

insert into axixs(sid,iid,aid) values(101,1001,10001);
insert into axixs(sid,iid,aid) values(101,1002,10001);
insert into axixs(sid,iid,aid) values(102,1003,10001);
insert into axixs(sid,iid,aid) values(102,1004,10001);
insert into axixs(sid,iid,aid) values(102,1005,10002);

