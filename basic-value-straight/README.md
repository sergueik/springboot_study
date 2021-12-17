### Info

this directory contains exercse from __How to Inject a Property Value Into a Class Not Managed by Spring?__ [article](https://www.baeldung.com/inject-properties-value-non-spring-class)

### Usage

* loading Springboot injected classed into plain does not trigger property  autowiring.

* one can directly load properties from the jar.
```sh
mvn spring-boot:run
```
```sh
curl  http://localhost:8085/basic/value
```
```text
Value got from the file
```

during the test an entirely test specific `application.properties` will be used:

```java
@Test
public void test1() {
	assertThat(controller.value(), is("Value for test"));
}

```

this test will pass, as well as this

```java
@Test
public void test2() {
	assertThat(utils.getValue(), is("Value for test"));
}
```

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
