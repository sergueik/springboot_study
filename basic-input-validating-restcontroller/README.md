### Info

Replica of [Spring-Rest-Field-Validation](https://github.com/Java-Gyan-Mantra/Spring-Rest-Field-Validation) with added Unit test exposing the underlying request validations.

### Testing
Test sends a payload of JSON serialized instane of a twin class `MalformedUser` with all field validator annotations cleared
```java
public class MalformedUser {

	private String name;
	private String email;
	private String password;


```
of the underlying POJO class `User` that is engaged with field validations
```java
public class User {

	@NotNull
	@Size(min = 2, max = 30, message = "Name should contains more than 2 character")
	private String name;

	@NotNull
	@Email(message = "Invalid Email")
	@Pattern(regexp = "\\b[\\w.%-]+@[-.\\w]+\\.[A-Za-z]{2,4}\\b", message = "Invalid Email")
	private String email;
...
```
and expects to receive validation errors when original class annotations are not met:
```java
@Test
public void test4() throws Exception {
	FieldValidationController controller = new FieldValidationController();
	Gson gsonPrinter = new GsonBuilder().setPrettyPrintinga();

	user = new MalformedUser("name", "invalid email", "a1b2c3d4");
	MockMvc mvc = MockMvcBuilders.standaloneSetup(controller).build();
	mvc.perform(post(route).contentType(MediaType.APPLICATION_JSON)
	.content(gsonPrinter.toJson(user))).andExpect(content().string(containsString("Invalid Email")));
}

```

### TODO

Upgrade from __1.5.4.RELEASE__ to later versions of SpringBoot

### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
