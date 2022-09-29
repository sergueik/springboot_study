package example;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import example.Application;

@SpringBootTest
@ContextConfiguration(classes = Application.class)
class ApplicationTest {

	// @Disabled("The forked VM terminated without properly saying goodbye")
	@Test
	void test() {
	}

}
