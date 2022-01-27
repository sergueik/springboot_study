### Info

This directory contains  a replica of [Yugabyte Spring CRUD demo](https://github.com/MikeQin/yugabyte-spring)

### Usage

* use the setup from the project [basic-yugabyte-rest](https://github.com/sergueik/springboot_study/tree/master/basic-yugabyte-rest)
```sh
docker pull yugabytedb/yugabyte:2.5.0.0-b2
```
```sh
 docker run -d --name yugabyte  -p 7000:7000 -p 9000:9000 -p 5433:5433 -p9042:9042 yugabytedb/yugabyte:2.5.0.0-b2 bin/yugabyted start --daemon=false
```
```sh
docker logs -f yugabyte
```
shows after a while
```text
Starting yugabyted...
✅ System checks
```
* launch the app

```sh
mvn -Dmaven.test.skip=true spring-boot:run
```
alternarively can abbreviate the options
and build the server package
```sh
mvn -DskipTests package
```

* run the REST API server:

```
mvn spring-boot:run
```

get interfaces information:
```sh
curl -q http://localhost:8080/ | jq '.'
```
```json
{
  "_links": {
    "users": {
      "href": "http://localhost:8080/users{?page,size,sort}",
      "templated": true
    },
    "products": {
      "href": "http://localhost:8080/products{?page,size,sort}",
      "templated": true
    },
    "orders": {
      "href": "http://localhost:8080/orders{?page,size,sort}",
      "templated": true
    },
    "orderLines": {
      "href": "http://localhost:8080/orderLines{?page,size,sort}",
      "templated": true
    },
    "profile": {
      "href": "http://localhost:8080/profile"
    }
  }
}

```
explore the data (originally there is none):
```sh
curl -s http://localhost:8080/users | jq '.'
```
```json
{
  "content": [],
  "pageable": {
    "sort": {
      "sorted": false,
      "unsorted": true,
      "empty": true
    },
    "pageNumber": 0,
    "pageSize": 20,
    "offset": 0,
    "unpaged": false,
    "paged": true
  },
  "last": true,
  "totalPages": 0,
  "totalElements": 0,
  "first": true,
  "sort": {
    "sorted": false,
    "unsorted": true,
    "empty": true
  },
  "numberOfElements": 0,
  "size": 20,
  "number": 0,
  "empty": true
}

```


### Customizing

There are a number of options that can be customized in the properties file located here:
[`src/main/resources/application.properties`](src/main/resources/application.properties)

| Properties    | Description   | Default |
| ------------- | ------------- | ------- |
| `spring.datasource.url`  | The connection string. | `jdbc:postgresql://localhost:5433/jq_db`  |
| `server.port`  | The port on which the REST API server should listen. | 8080 |
| `spring.datasource.username` | The username to connect to the database. | `yugabyte` |
| `spring.datasource.password` | The password to connect to the database. Leave blank for the password. | - |

### Data Injection

```sh
curl --data '{ "firstName" : "John", "lastName" : "Smith", "email" : "jsmith@yb.com" }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/users
```
```sh
curl --data '{ "firstName" : "Tom", "lastName" : "Stewart", "email" : "tstewart@yb.com" }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/users
```
```sh
curl --data '{ "productName": "Notebook", "description": "200 page notebook", "price": 7.50 }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/products

```
```sh
curl --data '{ "productName": "Pencil", "description": "Mechanical pencil", "price": 2.50 }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/products
```
```sh
curl --data '{ "userId": "2", "products": [ { "productId": 1, "units": 2 } ] }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/orders
```
```sh
curl --data '{ "userId": "2", "products": [ { "productId": 1, "units": 2 }, { "productId": 2, "units": 4 } ] }' -v -X POST -H 'Content-Type:application/json' http://localhost:8080/orders
```

### Querying the Data

### Using YSQL shell Console

```sh
CONTAINER_IMAGE=yugabyte
CONTAINER_NAME=yugabyte
CONTAINER_ID=$(docker container ls | grep $CONTAINER_IMAGE | awk '{print $1}')
DATABASE=yb_db
docker exec -it $CONTAINER_ID /home/yugabyte/bin/ysqlsh $DATABASE --echo-queries
```
```SQL
yugabyte=# \d
```
```sql
SELECT * FROM users;
```
this will show
```SQL
 user_id | first_name | last_name |   user_email
---------+------------+-----------+-----------------
       1 | Tom        | Stewart   | tstewart@yb.com
```
if other relations were data inserted the following will also show nonzero
```SQL
SELECT count(*) FROM products;
SELECT count(*) FROM orders;
SELECT * FROM orderline;
```

#### Using REST API

``sh
curl -s http://localhost:8080/users | jq '.'
```
```sh
curl -s http://localhost:8080/products | jq '.'
```
```sh
curl http://localhost:8080/orders | jq '.'
```
### See Also

  * [Yugabyte Spring Data Rest/JPA](https://github.com/MikeQin/yugabyte-spring-data-rest) demo
 
### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

