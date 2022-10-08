### Info

A replica of Spring Boot Application with Embeded PostgreSQL database [example](https://github.com/skpk24/springboot_embeded_postgresql)
sans devtools plugin since we do not intend to Dockerize it
### Usage
```cmd
mvn spring-boot:run
```
* note: maven would download a full ~200 Mb Postgresql [installer](https://www.postgresql.org/download/linux/ubuntu/) and cache it in
```sh
~/.embedpostgresql
```
#### Windows Specific Issues
* on Widows the application is not ntirely stable:

 + in first run fails with
```text
org.postgresql.util.PSQLException: Connection to localhost:63340 refused. Check that the hostname and port are correct and that the postmaster is accepting TCP/IP connections.
unable to open JDBC Connection for DDL execution
```
+ in the second run fails with

```text
2021-11-28 20:07:50.441  INFO 11256 --- [           main] d.f.embed.process.runtime.Executable     : start AbstractPostgresConfig{storage=Storage{dbDir=C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401, dbName='test', isTmpDir=true}, network=Net{host='localhost', port=63644}, timeout=Timeout{startupTimeout=15000}, credentials=Credentials{username='user', password='pass'}, args=[], additionalInitDbParams=[--nosync, --locale=en_US]}2021-11-28 20:07:52.444  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Possibly failed to run initdb:

The files belonging to this database system will be owned by user "Serguei".

This user must also own the server process.

initdb: invalid locale name "en_US"


2021-11-28 20:08:12.459 ERROR 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Failed to read PID file (File 'C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401\postmaster.pid' does not exist)java.io.FileNotFoundException: File 'C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401\postmaster.pid' does not exist
2021-11-28 20:08:17.373  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Could not create database first time (0 of 3 trials)
2021-11-28 20:08:18.005  INFO 11256 --- [           main] d.f.embed.process.runtime.Executable     : start AbstractPostgresConfig{storage=Storage{dbDir=C:\Users\Serguei\AppData\Local\Temp\postgresql-embed-36d90118-e071-43d5-b688-886260410419\db-content-85181475-027a-4481-9372-f61610856401, dbName='test', isTmpDir=true}, network=Net{host='localhost', port=63644}, timeout=Timeout{startupTimeout=15000}, credentials=Credentials{username='user', password='pass'}, args=[test], additionalInitDbParams=[]}2021-11-28 20:08:22.115  WARN 11256 --- [           main] r.y.q.embed.postgresql.PostgresProcess   : Could not create database first time (1 of 3 trials)
```
(fails all 3 trials)
 - removing 
 fixes this issue.
### Test
Testing the REST API urls
`http://localhost:8080/api/db/v1/data` and `http://localhost:8080/api/v1/ms` is successful:
```sh
curl -s http://localhost:8080/api/db/v1/data | jq '.[1]'
```
```json
{
  "id": 11,
  "civil": "Civil : 11",
  "nom": "Nom for : 11",
  "prenom": "Prenom : 11",
  "tel": "234-333-4411",
  "email": " test11@gmail.com",
  "fax": "123-233-3311",
  "addr1": "Address 1 for : 11",
  "addr2": "Address 2 for : 11",
  "addr3": "Address 3 for : 11",
  "codpos": "COD : 11",
  "ville": "Ville 11",
  "pays": "Pays : 11",
  "natact": " natact : 11",
  "dialang": "Dialang :  11",
  "ackmail": "0",
  "lastupdate": "2022-10-08T03:54:30.004+00:00",
  "lastupdateId": 31
}

```
```sh
curl -s http://localhost:8080/api/v1/ms | jq '.'
```
```json
{
  "ObjectDefinition": {
    "variables": {
      "variable": [
        {
          "mandatoryArray": false,
          "displayName": "view_name",
          "values": {
            "value": {
              "displayValue": "val1_disp",
              "content": 111
            }
          },
          "classes": {
            "class": [
              "string",
              "myclass"
            ]
          },
          "displayOrder": 0,
          "increment": 1,
          "fullDisplayName": "view_name",
          "localVarNameMatch": "",
          "type": "String",
          "displayNameHeader": "view_name",
          "arrayCanEdit": true,
          "groupDisplayName": "",
          "remoteVarNameMatch": "",
          "default": "L3",
          "arrayCanMove": true,
          "selector": false,
          "groupSeparator": "",
          "isSearchable": false,
          "keepOnImport": false,
          "isMandatory": true,
          "visible": true,
          "editable": true,
          "behaviors": {
            "behaviour": [
              {
                "displayName": "new_var0",
                "displayOrder": 0,
                "name": "params.new_var0",
                "startIncrement": false,
                "type": "String",
                "maxLength": 100,
                "selectorValue": "selector_val1"
              },
              {
                "displayName": "new_var2",
                "displayOrder": 0,
                "name": "params.new_var2",
                "startIncrement": false,
                "type": "String",
                "maxLength": 100,
                "selectorValue": "selector_val1"
              }
            ]
          },
          "startIncrement": 0,
          "isUniqueGlobal": false,
          "onlyDetailView": true,
          "isUserLocked": true,
          "sections": {
            "section": [
              "String",
              "mysection"
            ]
          },
          "arrayCanRemove": true,
          "refDeviceIdVar": "",
          "arrayCanAdd": true,
          "name": "params.view_name",
          "refServiceURI": "",
          "isGrouped": true
        },
        {
          "mandatoryArray": false,
          "displayName": "object_id",
          "values": {
            "value": {
              "displayValue": "val1_disp",
              "content": 111
            }
          },
          "classes": {
            "class": [
              "string",
              "myclass"
            ]
          },
          "displayOrder": 0,
          "increment": 1,
          "fullDisplayName": "object_id",
          "localVarNameMatch": "",
          "type": "String",
          "displayNameHeader": "object_id",
          "arrayCanEdit": true,
          "groupDisplayName": "",
          "remoteVarNameMatch": "",
          "default": "L3",
          "arrayCanMove": true,
          "selector": false,
          "groupSeparator": "",
          "isSearchable": false,
          "keepOnImport": false,
          "isMandatory": false,
          "visible": true,
          "editable": true,
          "behaviors": {
            "behaviour": [
              {
                "displayName": "new_var0",
                "displayOrder": 0,
                "name": "params.new_var0",
                "startIncrement": false,
                "type": "String",
                "maxLength": 100,
                "selectorValue": "selector_val1"
              },
              {
                "displayName": "new_var2",
                "displayOrder": 0,
                "name": "params.new_var2",
                "startIncrement": false,
                "type": "String",
                "maxLength": 100,
                "selectorValue": "selector_val1"
              }
            ]
          },
          "startIncrement": 0,
          "isUniqueGlobal": false,
          "onlyDetailView": false,
          "isUserLocked": true,
          "sections": {
            "section": [
              "String",
              "mysection"
            ]
          },
          "arrayCanRemove": true,
          "refDeviceIdVar": "",
          "arrayCanAdd": true,
          "name": "params.object_id",
          "refServiceURI": "",
          "isGrouped": false
        }
      ],
      "frozen": 0
    },
    "information": {
      "visibility": 0,
      "icon": "none",
      "sortvariable": "param._order",
      "description": "",
      "importrank": 0,
      "configType": "",
      "sortascending": false,
      "sortnumerical": false,
      "createTemplateObject": "",
      "importIfMandatoryPresent": false,
      "createTemplateId": 0,
      "defaultDisplay": false,
      "name": "myMS123466",
      "displayField": "none",
      "dynamic": false,
      "reorderinstances": false,
      "maxInstances": 0,
      "relatedObjects": "",
      "sortauto": false,
      "order": 0,
      "group": "Default",
      "importonce": false
    },
    "example": "none"
  }
}

```
### See Also

 * https://github.com/xcwy/springboot-embedded-postgresql
 * https://github.com/Romeh/springboot-postgres-embedded-dao-testing 

### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
