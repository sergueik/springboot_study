package example;

import com.intuit.karate.junit5.Karate;

public class ExampleTest {

	@Karate.Test
	Karate testUi() {
		return Karate.run("classpath:example/feature/Test.feature");
	}

}
