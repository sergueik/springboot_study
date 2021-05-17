package example.component;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

// https://stackoverflow.com/questions/17353327/populating-spring-value-during-unit-test
// https://www.baeldung.com/spring-tests-override-properties

@SpringBootTest
public class ExampleComponentTest {

	@Autowired
	private ExampleComponent sut;
	String property;

	@Test
	public void test1() {
		property = sut.getProperty1();
		assertThat("unexpected value of property1: " + property, property,
				is("testdata"));
	}

	@Test
	public void test2() {
		property = sut.getProperty2();
		assertThat("unexpected value of property2: " + property, property,
				is("default"));
	}

	@Test
	public void test3() {
		property = sut.getProperty3();
		assertThat("unexpected value of property3: " + property, property,
				is("/tmp"));
	}

	@Test
	public void test4() {
		property = sut.getProperty4();
		assertThat("unexpected value of property4: " + property, property,
				is("d:\\temp"));
	}
}
