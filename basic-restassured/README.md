### Info

refactored example [project](https://github.com/swtestacademy/RestAssuredExample) using [one](https://gorest.co.in)
of many available free dummy JSON REST API sites
### Usage

The following RestArrured signature extensively builder-pattern chained code with logging (NOTE: cannot chain the `log()` call afrer `path()` call):
```java
@Test
public void test() {
  result = RestAssured.when().get(path).then().statusCode(statusCode)
      .assertThat().log().all().extract().path(search);
  assertThat(result, containsString(name));		
  System.out.println("result: " + result);
}
```
produces the same results as plain Java code
```java
@Test
public void test() {
  response = RestAssured.get(path);
  assertThat(response.statusCode(), is(statusCode));
  data = response.asString();
  assertThat(data, notNullValue());
  System.out.println("response: " + data);
  result = new JsonPath(data).get(search).toString();
  assertThat(result, containsString(name));
  System.out.println("returned: " + result);
}
```
NOTE: use `System.out` and not `System.err` to maintain log order
### See Also 
  * http://makeseleniumeasy.com/2019/11/14/rest-assured-tutorial-7-builder-pattern-in-rest-assured/

NOTE: for older versions use `com.jayway.restassured` group id  for `rest-assured` and `json-schema-validator`

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
