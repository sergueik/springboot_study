### Info

simplified replica of [repository](https://github.com/talk2debendra/spring-boot)
illustrating the [article](https://talk2debendra90.medium.com/spring-data-jpa-query-by-example-qbe-a9c817248c0d)



### Usage

* currently there is no unit tests, just a dummy `contextLoads` one
 one so to test need to
```sh
mvn spring-boot:run
```
then perform queries
```sh
curl -s http://localhost:8080/customer/firstname?endsWith=dra | /c/tools/jq-win64.exe '.'
```
```JSON
[
  {
    "id": 1,
    "firstName": "Debendra",
    "lastName": "Dhinda",
    "walletBalance": 5000
  },
  {
    "id": 10,
    "firstName": "Jitendra",
    "lastName": "Bhandari",
    "walletBalance": 2000
  },
  {
    "id": 14,
    "firstName": "Mahendra",
    "lastName": "Khillar",
    "walletBalance": 5000
  },
  {
    "id": 15,
    "firstName": "Narendra",
    "lastName": "Jindal",
    "walletBalance": 2000
  }
]

```
```sh
curl -s http://localhost:8080/customer/firstname?endsWith=dra | /c/tools/jq-win64.exe '.' | /c/tools/jq-win64.exe '.'
```
```JSON
[
  {
    "id": 1,
    "firstName": "Debendra",
    "lastName": "Dhinda",
    "walletBalance": 5000
  },
  {
    "id": 10,
    "firstName": "Jitendra",
    "lastName": "Bhandari",
    "walletBalance": 2000
  },
  {
    "id": 14,
    "firstName": "Mahendra",
    "lastName": "Khillar",
    "walletBalance": 5000
  },
  {
    "id": 15,
    "firstName": "Narendra",
    "lastName": "Jindal",
    "walletBalance": 2000
  }
]
```
```sh
curl -s http://localhost:8080/customer/firstname?startsWith=An | /c/tools/jq-win64.exe '.'
```
```JSON
[
  {
    "id": 3,
    "firstName": "Anita",
    "lastName": "Samal",
    "walletBalance": 5000
  },
  {
    "id": 4,
    "firstName": "Anita",
    "lastName": "Das",
    "walletBalance": 3000
  },
  {
    "id": 11,
    "firstName": "Ankit",
    "lastName": "Jindal",
    "walletBalance": 2000
  }
]
```
NOTE: empty argument is  supported by `@Query` version (testing thr QBE code is a work in progress):
```sh
curl -s http://localhost:8080/customer/firstname?startsWith=""|/c/tools/jq-win64.exe  '.|length'
```
```text
15
```


* exact name

```sh
curl http://localhost:8080/customer/rosalin
```

```JSON
[{"id":13,"firstName":"Rosalin","lastName":"Khillar","walletBalance":2000}]
```
* regexp
```sh
curl  -s "http://localhost:8080/customer/query?firstName=S.*" | /c/tools/jq-win64.exe  '.'
```
```JSON
[
  [
    2,
    "Satvik",
    "Dhinda",
    2000
  ],
  [
    8,
    "Shyambrij",
    "Mourya",
    1000
  ],
  [
    12,
    "Soumya",
    "Shankar",
    1000
  ]
]
```
* sorting via custom code
```sh
curl  -s "http://localhost:8080/customer" | /c/tools/jq-win64.exe  "."
```
```JSON
[
  {
    "id": 15,
    "firstName": "Narendra",
    "lastName": "Jindal",
    "walletBalance": 2000
  },
  {
    "id": 14,
    "firstName": "Mahendra",
    "lastName": "Khillar",
    "walletBalance": 5000
  },
  {
    "id": 13,
    "firstName": "Rosalin",
    "lastName": "Khillar",
    "walletBalance": 2000
  },
  {
    "id": 12,
    "firstName": "Soumya",
    "lastName": "Shankar",
    "walletBalance": 1000
  },
  {
    "id": 11,
    "firstName": "Ankit",
    "lastName": "Jindal",
    "walletBalance": 2000
  },
  {
    "id": 10,
    "firstName": "Jitendra",
    "lastName": "Bhandari",
    "walletBalance": 2000
  },
  {
    "id": 9,
    "firstName": "Harikant",
    "lastName": "Ojha",
    "walletBalance": 5000
  },
  {
    "id": 8,
    "firstName": "Shyambrij",
    "lastName": "Mourya",
    "walletBalance": 1000
  },
  {
    "id": 7,
    "firstName": "Kuber",
    "lastName": "Goel",
    "walletBalance": 5000
  },
  {
    "id": 6,
    "firstName": "Vidushi",
    "lastName": "Diwedy",
    "walletBalance": 3000
  },
  {
    "id": 5,
    "firstName": "Promod",
    "lastName": "Barik",
    "walletBalance": 2000
  },
  {
    "id": 4,
    "firstName": "Anita",
    "lastName": "Das",
    "walletBalance": 3000
  },
  {
    "id": 3,
    "firstName": "Anita",
    "lastName": "Samal",
    "walletBalance": 5000
  },
  {
    "id": 2,
    "firstName": "Satvik",
    "lastName": "Dhinda",
    "walletBalance": 2000
  },
  {
    "id": 1,
    "firstName": "Debendra",
    "lastName": "Dhinda",
    "walletBalance": 5000
  }
]


```
* sorting without adding code (only the interface):
```java
@Repository
public interface CustomerRepository extends JpaRepository<Customers, Long> {
	public List<Customers> findAllByOrderByIdDesc();

```
```sh
curl  -s "http://localhost:8080/customer/sorted?auto=true" | /c/tools/jq-win64.exe  "."
```
the log will show

```text
2023-04-07 18:41:34.844  INFO 7392 --- [nio-8080-exec-5] example.controller.CustomerController    : Request received to get all available customers sorted in descending order by JPA
```

### Comparison

Alternatively may use classic `@Query` annotations and projection classes (unfinished, using th original model class )

```java
// NOTE: strongly typed
@Query("SELECT new example.model.Customers(a.id, a.firstName,a.lastName,a.walletBalance)"
		+ " from Customers a where a.firstName LIKE ?1%")
public List<Customers> findCustomers(String firstNameFragment);

```
### TODO

Problem adding unit tests in the project

```text
Caused by: org.springframework.beans.factory.NoSuchBeanDefinitionException: No bean named 'entityManagerFactory' available
```
the attempt to apply fix by [stackoverflow hint](https://stackoverflow.com/questions/24520602/spring-data-jpa-no-bean-named-entitymanagerfactory-is-defined-injection-of-a)
leads to 
```text
Caused by: java.lang.IllegalArgumentException: Not a managed type: class example.model.Customers
```
### See Also
  * [delombok](https://projectlombok.org/features/delombok)
  * [lombok](https://www.baeldung.com/lombok-builder) `@Builder` annotation
  * [Spring Data JPA Query by Example](https://www.baeldung.com/spring-data-query-by-example)
  * [explicit sorting with code](https://stackoverflow.com/questions/25486583/how-to-use-orderby-with-findall-in-spring-data)
  * [Springboot h2 database](https://www.baeldung.com/spring-boot-h2-database)
  * [sorting with JPA](https://www.baeldung.com/jpa-sort)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
