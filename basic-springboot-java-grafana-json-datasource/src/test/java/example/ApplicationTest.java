package example;

// NOTE: the dummy "contextLoads" test class has to belong to the same package
// as the application or will fail with
// java.lang.IllegalStateException: Unable to find a @SpringBootConfiguration, 
// you need to use @ContextConfiguration or @SpringBootTest(classes=...)
// with your test

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest

public class ApplicationTest {

	// @Disabled("The forked VM terminated without properly saying goodbye")
	@Test
	public void contextLoads() {
	}

}
