### Info 

replica of [h2 jbc project](https://github.com/kuidreS/JDBC) with some fixes of typos

### Usage

* create tables via h2 web console
```sh
java -jar ~/.m2/repository/com/h2database/h2/2.1.210/h2-2.1.210.jar
```
in `jdbc:h2:~/test`
with user `ss`, blank password
```text
CREATE TABLE "ADDRESS" ( "ID" bigint NOT NULL, "COUNTRY" character varying(255) NOT NULL, "CITY" character varying(255) NOT NULL, "STREET" character varying(255) NOT NULL, "POST_CODE" character varying(10) NOT NULL, CONSTRAINT "ID" PRIMARY KEY ("ID") )
```
```text

DROP TABLE "EMPL_PROJ";
DROP TABLE "EMPLOYEE";

CREATE TABLE "EMPLOYEE" ( 
"ID" bigint NOT NULL, 
"FIRST_NAME" character varying(255) NOT NULL, 
"LAST_NAME" character varying(255) NOT NULL, 
"BIRTHDAY" date NOT NULL, 
"ADDRESS_ID" bigint NOT NULL, 
CONSTRAINT "EMPLOYEE_pkey" PRIMARY KEY ("ID"),
CONSTRAINT "EMPLOYEE_ADDRESS_ID_fkey" FOREIGN KEY ("ID") REFERENCES "ADDRESS" ("ID") 
)
```
```text
CREATE TABLE "PROJECT" ( "ID" bigint NOT NULL, "TITLE" character varying(255) NOT NULL, CONSTRAINT "PROJECT_pkey" PRIMARY KEY ("ID") )
```
```text
CREATE TABLE "EMPL_PROJ" ( 
"EMPLOYEE_ID" bigint NOT NULL, 
"PROJECT_ID" bigint NOT NULL, 
CONSTRAINT "EMPL_PROJ_EMPLOYEE_ID_fkey" 
FOREIGN KEY ("EMPLOYEE_ID") REFERENCES "EMPLOYEE" ("ID"), 
CONSTRAINT "EMPL_PROJ_PROJECT_ID_fkey" 
FOREIGN KEY ("PROJECT_ID") REFERENCES "PROJECT" ("ID") )
```
* close the h2 connection

* package app
```sh
mvn package
```
* run app
```sh
java -cp target/h2-jdbc-tutorial-0.1.0-SNAPSHOT.jar:target/lib/*  Domain
```
### Cleanup
```sh
rm ~/test.*.db
```
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
