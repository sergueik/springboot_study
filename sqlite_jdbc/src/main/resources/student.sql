exec sys.sp_readerrorlog 0, 1, 'listening';
create database student;
use student;

create table student(
id      bigint primary key identity(1,1),
name    NVARCHAR(30) not null,
course	NVARCHAR(30) not null,
addtime datetime not null default current_timestamp);

insert into student(name,course)values('Jack','Chinese');
insert into student(name,course)values('Tom','Computer');


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


insert into server(sname,sid) values('server01',101);
insert into server(sname,sid) values('server02',102);
insert into server(sname,sid) values('server03',103);

insert into application(aname,aid) values('application01',10001);
insert into application(aname,aid) values('application02',10002);
delete from application where aid = 10002;


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


select sname,iname,aname from axixs x join server s on x.sid = s.sid join application a on x.aid = a.aid join instance i on x.iid = i.iid

