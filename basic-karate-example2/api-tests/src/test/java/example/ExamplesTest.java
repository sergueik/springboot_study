package example;

import com.intuit.karate.Results;
import com.intuit.karate.Runner;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

class ExamplesTest {

	@Test
	void testParallel() {
		Results results = Runner.path("classpath:example")
				// .outputCucumberJson(true)
				.parallel(5);
		assertEquals(0, results.getFailCount(), results.getErrorMessages());
	}

}
