### Info

simplified replica of [repository](https://github.com/talk2debendra/spring-boot) illustrating the [article](https://talk2debendra90.medium.com/spring-data-jpa-query-by-example-qbe-a9c817248c0d)



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
```json
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
```json
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
```json
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
### Comparison

Alternatively may use classic `@Query` annotations and projection classes (unfinished, using th original model class )

```java
// NOTE: strongly typed
@Query("SELECT new example.model.Customers(a.id, a.firstName,a.lastName,a.walletBalance)"
		+ " from Customers a where a.firstName LIKE ?1%")
public List<Customers> findCustomers(String firstNameFragment);

```
### See Also
  * [delombok](https://projectlombok.org/features/delombok)
  * [lombok](https://www.baeldung.com/lombok-builder) `@Builder` annotation
  * [Spring Data JPA Query by Example](https://www.baeldung.com/spring-data-query-by-example)

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
