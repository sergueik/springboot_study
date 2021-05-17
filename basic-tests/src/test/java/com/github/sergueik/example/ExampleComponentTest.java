package com.github.sergueik.example;

import static org.hamcrest.MatcherAssert.assertThat;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;

import org.junit.jupiter.api.Disabled;
//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;

import com.github.sergueik.example.ExampleComponent;

@ContextConfiguration(classes = com.github.sergueik.example.ExampleComponent.class)
@SpringBootTest(classes = com.github.sergueik.example.ExampleComponent.class)
public class ExampleComponentTest {

	@Autowired
	private ExampleComponent sut;
	String property;

	@Test
	public void test1() {
		property = sut.getProperty1();
		assertThat("unexpected value of property1: " + property, property, is("test data"));
	}

	@Test
	public void test2() {
		property = sut.getProperty2();
		assertThat("unexpected value of property2: " + property, property, is("default"));
	}

	@Test
	public void test3() {
		property = sut.getProperty3();
		assertThat("unexpected value of property3: " + property, property, is("/tmp"));
	}

	@Test
	// get public member directly
	public void test4() {
		property = sut.property5;
		assertThat("unexpected value of property5: " + property, property, is("default"));
	}

	// @Disabled
	@Test
	// unexpected value of property6: null Expected: is "default"
	// see also:
	// https://www.baeldung.com/spring-inject-static-field
	public void test5() {
		property = sut.property6;
		assertThat("unexpected value of property6: " + property, property, is("static test data"));
		System.err.println("Check: ExampleComponent.property6=" + ExampleComponent.property6Static);
		property = ExampleComponent.property6Static;
		assertThat(property, notNullValue());
		assertThat("unexpected value of property6Static: " + property, property, is("static test data"));
	}

}
