### Info

this directory contains a trimmed down replica of 
__Reactive Spring Boot, Spring Webflux, Spring Data R2DBC application with PostgreSQL__ [project](https://github.com/pictet-technologies-open-source/reactive-todo-list-r2dbc)
 - no UI (the original project required `npm` to build the UI)

### Usage

* build the backend `app` (currently skipping the frontend `ui`)
```sh
mvn clean package
```

*  package and run in the container
```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose -f docker-compose.yml up --build
```

 *  test
```sh
curl -X GET http://localhost:8080/items 
```
this will print
```text
data:{"id":1,"version":1,"description":"Flight to JNB","status":"TODO","assignee":{"id":1,"firstName":"Richard","lastName":"Countin"},"tags":[{"id":2,"version":1,"name":"Private","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"},{"id":7,"version":1,"name":"Vacation","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}],"createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}
data:{"id":2,"version":1,"description":"Organise an celebration for the Great Place to Work","status":"TODO","assignee":{"id":2,"firstName":"Nathalie","lastName":"Queen"},"tags":[{"id":6,"version":1,"name":"Drink","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"},{"id":5,"version":1,"name":"Meal","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"},{"id":3,"version":1,"name":"Meeting","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"},{"id":1,"version":1,"name":"Work","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}],"createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}
data:{"id":3,"version":1,"description":"Organise a drink with the team","status":"IN_PROGRESS","assignee":{"id":3,"firstName":"Benito","lastName":"Corazon"},"tags":[{"id":6,"version":1,"name":"Drink","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"},{"id":1,"version":1,"name":"Work","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}],"createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}
data:{"id":4,"version":1,"description":"Meet Ms Cosmic","status":"DONE","assignee":{"id":4,"firstName":"Vince","lastName":"Power"},"tags":[{"id":2,"version":1,"name":"Private","createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}],"createdDate":"2023-07-18T17:58:55.7662","lastModifiedDate":"2023-07-18T17:58:55.7662"}
```

```sh
curl -sX GET http://localhost:8080/items/1 |jq '.'
```
```JSON
{
  "id": 1,
  "version": 1,
  "description": "Flight to JNB",
  "status": "TODO",
  "assignee": {
    "id": 1,
    "firstName": "Richard",
    "lastName": "Countin"
  },
  "tags": [
    {
      "id": 2,
      "version": 1,
      "name": "Private",
      "createdDate": "2023-07-18T17:58:55.7662",
      "lastModifiedDate": "2023-07-18T17:58:55.7662"
    },
    {
      "id": 7,
      "version": 1,
      "name": "Vacation",
      "createdDate": "2023-07-18T17:58:55.7662",
      "lastModifiedDate": "2023-07-18T17:58:55.7662"
    }
  ],
  "createdDate": "2023-07-18T17:58:55.7662",
  "lastModifiedDate": "2023-07-18T17:58:55.7662"
}

```
* do a successful `POST` request
```sh
curl -H 'Content-Type: application/json' -X POST -d '{"version": 1, "description": "Test", "status": "TODO",  "tags": [ ]}' http://localhost:8080/items 
```
* do a failing `POST` request
```
curl -H 'Content-Type: application/json' -H 'If-Match: 0' -X PUT -d '{ "version":2, "description": "Test", "status": "TODO",  "tags": [ ]}' http://localhost:8080/items/100
```
NOTE: without the `If-Match` header this will respond with

```JSON
{
  "timestamp": "2023-07-18T20:11:46.344+00:00",
  "status": 400,
  "error": "Bad Request",
  "message": "",
  "path": "/items/100"
}

```

and with value in the `If-Match` header not match the existing item `version` the error will be:

```text
{
  "timestamp": "2023-07-18T20:38:07.716+00:00",
  "status": 412,
  "error": "Precondition Failed",
  "message": "",
  "path": "/items/100"
}
```


alternatively perform the operations through Swagger UI page `http://localhost:8080/swagger-ui/#`

* bring pgadmin up (optional)

```sh
export COMPOSE_HTTP_TIMEOUT=600
docker-compose -f docker-compose-postgres.yml up --build
```

* connect through web interface `http://localhost:5050/login?next=%2F with `admin@admin.com`/`password` credentials
![Login](https://github.com/sergueik/springboot_study/blob/master/basic-r2dbc/screenshsots/capture-pgadmin4.png)


* register connection to `postgresdb`
![Register Connection](https://github.com/sergueik/springboot_study/blob/master/basic-r2dbc/screenshots/capture-register.png)

* browse to the schema and table in question
![Browse Schema](https://github.com/sergueik/springboot_study/blob/master/basic-r2dbc/screenshots/capture-table.png)

###  See Also:
  * [Reactive Programming and Relational Databases](https://spring.io/blog/2018/12/07/reactive-programming-and-relational-databases/)
  * [R2DBC](https://r2dbc.io)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
